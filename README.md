## About Qtools
Qtools is a collection of utilities to help development with CommonQt. There are a lot of things in there, from name mapping over garbage handling to widget class definition. Some tools are straightforward, others are quite complex. I'll try to explain everything as best I can.

## How To
For Qtools to work you'll obviously need a working [CommonQt](http://common-lisp.net/project/commonqt/). Knowing a bit about CommonQt and Qt development will also help in understanding these tools.

### Basic Tools

#### Names
Since Qt is a C++ library we're dealing with different naming conventions and different type designators. Qtools offers some help in dealing with both. Mapping more lisp-y class names to the equivalent Qt class names can be done with `find-qt-class-name`. It translates things like the symbol `qwidget`, or even `q-widget` into `"QWidget"`. In order to guess the correct capitalisation it uses a predefined set of names from `*qt-class-map*`.

Translating a lispy function name symbol into a C++ method name can be done with `to-method-name`. Example, `to-method-name` would become `"toMethodName"`.

C++/Qt also has a different set of types than CL, but we need the type names to define things like signals and slots. The functions `qt-type-of`, `qt-type-for`, `to-type-name`, `cl-type-for`, `eqt-type-of` and `ecl-type-for` can help with that by attempting to translate between the two worlds. Using these may be dangerous, as the type information might be ambiguous or no proper translation exists at all, so the results might not always be exactly what you need.

Also useful for full method name (including argument types) translation are `determined-type-method-name` and `specified-type-method-name`.

#### Other Stuff
Aside from name handling, Qtools also offers some convenience functions like `copy-qobject`, `maybe-delete-qobject` and `qtenumcase` to ease handling of Qt objects.

### Working Without GC
One major headache when working with CommonQt is the lack of garbage collection for Qt objects. This is something that is unavoidable, short of adding a garbage collector C++-side, and even then synchronising the two worlds would be a big undertaking. Qtools attempts to ease the pain by providing functions to at least have some form of GC, or in the very least minimise the need to keep track of instances.

Central to this effort is the `finalize` generic function. This function should be called on any object that you want to discard. By default, objects of type `abstract-qobject` and `finalizable` are specially handled. Qobjects are automatically deleted so they won't linger on the C++ side and `finalizable`s are taken care of by calling `finalize` on all their `finalized` slots.

The `finalizable` class (which you can subclass with `define-finalizable`) offers a different slot type with the extra argument `:finalizable`. If that argument is non-NIL, the slot's value is `finalize`d when the object is `finalize`d. Of course you don't need to use the `finalized` class and can instead define methods for your own classes to handle cleanup explicitly.

If you only need to have certain objects around for the duration of a let body or something similar, you may find the `with-finalizing` and `with-finalizing*` functions of use.

As an attempt to automate garbage collection, you can also take advantage of the `gc-finalized` object. This object is a container for another object. Once the gc-finalized object loses all its references and is then at some point hit by the GC, `finalize` is automatically invoked on the contained object. For this to work properly, the container needs to have references for as long as you need the inner object. This might be dangerous if, for example, your contained object is referenced on the C++ side, but the container loses all its references on the CL side. That would result in the C++ object being finalized (and thus deleted), which might cause instability. Always be sure to keep the references in sync if you use the `gc-finalized` object.

However, the `gc-finalized` object can be useful when used with `with-gc-finalized`, as an alternative to the `with-finalizing` macro. With this macro, the values are automatically wrapped in a gc-finalized object, but also made convenient by providing a symbol-macro to automatically unwrap the contained object so it feels like a standard let. Once the block exits the objects are then not immediately finalized, but instead the containers lose their references and are thus garbage collected whenever the gc next hits.

Even with these tools, keeping track of objects is a pain and something you'll have to be careful about. I wish there was a way to have full garbage collection, but alas, life is difficult.

### Signals
Qtools offers some small macros to wrap around CommonQt's direct exposing of C++ names and types. This is especially noticeable with signals (the `connect` and `emit-signal` functions). To make these more lispy, you may use `connect!` and `signal!`:

    (connect! origin (signal-name int double) target (slot-name int double))
    (signal! target slot-name (0 int) (1.0 double))

These still require explicit type declaration though. Unfortunately, this is often necessary due to ambiguity or type mismatch. If you're feeling lucky, you may use `generic-signal`, which attempts to statically and dynamically determine the proper types for its arguments:

    (generic-signal target 'slot-name 0 1.0)

If you want to use `generic-signal` to dynamically determine only some arguments, you can wrap the ones you want to declare explicitly in a list:

    (generic-signal target 'slot-name dynamic `(,fixed double))

### The Widget Class
CommonQt's default class adds options that are necessary for proper Qt integration, such as overrides, slots and signals. However, the usage thereof is a big annoying. Qtools offers a separate metaclass/class pair that should handle this much more elegantly. In order to define widgets, you should use `define-widget`. This will automatically set the proper metaclass and superclasses:

    (define-widget my-widget (QWidget)
      ())

As you can see, it doesn't require string-escaping the Qt class name, as it can leverage `find-qt-class-name`. Aside form this minor convenience in definition shortage, a widget class keeps track of a few more things for you, which allow changing of class options outside of the definition form itself. This is useful precisely because it allows us to use a much more lispy approach to widget definition that would ordinarily be possible. With widgets you have access to `define-signal`, `define-slot`, `define-override`, `define-initializer`, `define-finalizer`, `define-subwidget`, and `define-menu` that Qtools provides out of the box:

    (define-signal (my-widget chant) (string))

    (define-slot (my-widget hear) ((thing string))
      (format T "I heard: ~s" thing))

    (define-slot (my-widget chant) ()
      (signal! my-widget chant ("MORE CODE" string)))

    (define-subwidget (my-widget chant-button) (#_new QPushButton "Chant!")
      (connect! chant-button (released) my-widget (chant)))
    
### The `defmethod`
Qtools provides its own version of `defmethod`. Now, I understand and agree that usually it is a bad idea to provide alternate standard functions as it will cause confusion. Here's the trick to Qtools' version though: It will act exactly like the standard `defmethod` (in fact, it will emit such a form) and only gives special treatment to select `declare` forms. This allows us to re-use the existing syntax that everyone already knows, but extend it by introducing new declarations, which won't clash otherwise. By default, the declarations for `slot`, `override`, `initializer`, and `finalizer` are recognised. These cause the effect that you might expect: They each modify the class the method specialises on and introduce the necessary machinery to connect the method call.

    (defmethod paint ((widget foo) paint-event)
      (declare (override paint-event))
      ...)

In order to be able to use this effectively and painlessly, you should probably `:use` the extra package `cl+qt` instead of `cl` in your package. This package exports `cl`, `qt`, and `qtools` with the proper shadowing in place to use Qtools' `defmethod`.

### Menu Definition
As mentioned above, Qtools also offers a `define-menu` macro which allows you to write menus in an extremely compact format:

    (define-menu (my-widget File)
      (:item ("Open..." (ctrl o))
        (open-file))
      (:menu recent-files)
      (:separator)
      (:item ("Save" (ctrl s))
        (save-file))
      (:item ("Save As..." (ctrl alt s))
        (save-file NIL))
      (:menu "Export"
        (:item "PNG" (save-file NIL "png")))
      (:separator)
      (:item ("Quit" (ctrl q))
        (#_close widget)))

In the case of items and menus, if the first argument is a symbol, it picks the object from the according slot on the widget. Otherwise it expects a string to use as text, or in the case of an item a string and a mnemonic. For items the body can be arbitrary lisp forms to be executed when the item is triggered. Menus can also be nested to arbitrary depth. All items that are created in a menu are also automatically stored in a list of QAction objects. You can retrieve this list for any class using `widget-actions`.

A frequent task is allowing the user to redefine the keyboard shortcuts of menu items. Since Qtools' `define-menu` keeps track of all its items, it is easy to inspect them and change their mnemonics. However, there's also the `keychord-editor` widget class, which should offer you a simple but effective dialog to change keyboard shortcuts.

### Readtable
CommonQt provides a necessary readtable to add a convenient way to write foreign calls. Qtools provides its own named-readtable (`:qtools`) that inherits from this readtable, but adds some minor tweaks.

The reader macros `#<` to call `unbox` and `#>` to call `make-gc-finalized` on the following object are available.

The Qtools readtable also deals with the Q+ system.

### Q+
By default with CommonQt, calling Qt methods happens with the `#_` reader macro. This requires you to follow the proper case of the class and method names. Having this kind of mixture of conventions in the code is a bit jarring. While Qtools offers solutions to deal with the discrepancies of defining your own classes and widgets using the various `define-*` macros, Q+ fixes the method calling discrepancy. In order to use Q+ you have a choice of either using the `q+` macro, or using the `:qtools` read-table. Using the `q+` macro directly an example translates like this:

    (let ((widget (#_new QWidget)))
      (#_setWindowTitle widget "Hello!")
      (#_show widget)
      (#_exec *qapplication*))

    (let ((widget (q+ make-qwidget)))
      (q+ set-window-title widget "Hello!")
      (q+ show widget)
      (q+ exec *qapplication*))

And the same using the readtable:

    (let ((widget (q+:make-qwidget)))
      (q+:set-window-title widget "Hello!")
      (q+:show widget)
      (q+:exec *qapplication*))

The difference is minimal in the typed code. However, the second approach will give you the convenience of letting the editor display the possible arguments and a docstring linking to the Qt methods. The second example is read by the Common Lisp reader to the first example. There is therefore no code difference in how the two work. If you use the `cl+qt` package, you can also take advantage of an extended `setf` macro. Using it, the second line would look like so:

    (setf (q+ window-title widget) "Hello!")

Some of the setter functions require multiple values to be set at once. The updated `setf` can also deal with that:

    (setf (q+ window painter) (values 0 0 100 100))

The `setf` has extra support for `q+`, but is otherwise identical to `cl:setf` and actually expands to that for all other places.

In order for Q+ to work seamlessly in conjunction with ASDF systems and compiling/loading code, you have to make sure that the smoke modules are set up correctly

### Smoke Modules
CommonQt uses the SmokeQt library to interface with Qt. Smoke is divided up into different modules that provide parts of the Qt framework. In order to be able to use the various parts of Qt, these modules need to be loaded. By default CommonQt loads `qtcore` and `qtgui` when `make-qapplication` is called. However, if you want to use, say, the OpenGL parts you'll also need `qtopengl`.

Qtools provides ASDF systems for all the different smoke modules. That way, you can simply push the modules you want into your project's ASDF system dependencies and it'll ensure that the modules are available at compile and load time. Having the modules loaded at both times is especially important for Q+ to work properly. An example system making use of this would look like

    (asdf:defsystem foo
      ...
      :depends-on (:qtcore :qtgui))

For a list of available smoke modules, see `*smoke-modules*`.

## Extending Qtools

### Copying and Finalizing
In order to account for your own objects and operations you can extend the `copy` and `finalize` functions by using `define-copy-method` and `define-finalize-method` or `defmethod` directly. The two define macros bring the convenience of automatically resolving to a Qt class (and thus using `copy/finalize-using-class`) if possible, making it all look a bit cleaner.

    (define-copy-method (instance QPixmap)
      "Creates a new QPixmap using QPixmap::copy (deep copy)."
      (#_copy instance (#_rect instance)))

Since copying and finalizing are operations associated with a certain amount of ambiguity, it is advisable to always write documentation strings for your `copy`/`finalize` methods. That way users can get a better idea of what will happen by reading about it using `describe-copy-method` and `describe-finalize-method` respectively.

### Adding `defmethod` declarations
Using `define-method-declaration` you can add your own processing to method declarations. Your function should extract the necessary information from its declaration arguments and the `*method*` form. Each method declaration processing function should return a single form (like a macro) to be put before the resulting `defmethod`. The existing declaration processors are really short:

    (define-method-declaration override (&optional name)
      (let ((slot (qtools:to-method-name (or name (form-fiddle:lambda-name *method*)))))
        (with-widget-class (widget-class)
          `(set-widget-class-option ',widget-class :override '(,slot ,name)))))

### Extending the menu definition
The menu definition form allows for arbitrary content types, so you may add new ones yourself by using `define-menu-content-type`. Each content type definition can return two values: an initform and a side-form. The initform will be put into the initialization function for the menu and thus evaluated when the widget is created. The side-form is put alongside the initializer definition and thus evaluated during compilation. If your menu type needs to modify the widget class in some way, that should be done through the side-forms. If it needs to connect signals, add items, or perform similar actions that involve Qt, that should go into the initform. You can call the expansion of other component types using `build-menu-content`. During the time your content-type function is run, `*widget*` is bound to the class-name of the widget and during initialization it is bound to the actual widget instance.

## Debugging Qtools
Since Qtools does a bunch of contrived things, you might want to check what exactly is done if something doesn't go according to plan. I'm not excluding the possibility of bugs being around that mess your code up. In order to check this, you will want to load [verbose](http://shinmera.github.io/verbose/) before loading Qtools and set the logging level to trace: `(setf (v:repl-level) :trace)`. Qtools will emit log messages when you compile `define-widget` forms that contain the generated options. It will also log all objects that get passed to `finalize` and `copy`. Hopefully the log output will help you in discovering what's going on behind the scenes.

## Qtools Concepts
Qtools has grown to be a large library with a lot of rather complicated concepts. I will try to describe them here, in order to retain some information in non-code form and make things clearer to the average user. It is not necessary to read and understand this section to use Qtools, but it may be useful to be aware of the underlying ideas and functionality that make Qtools work.

### Finalizables
A finalizable is implemented using two classes, one serving as the metaclass and the other as a superclass. The metaclass is required in order to allow a custom slot type that supports the `:finalized` argument. The superclass is necessary in order to allow methods such as `finalize` to operate on instances of the finalizable classes. The handling of the finalized slots is done through a general method on `finalize` that scans through the slots of the class instance and then calls `finalize` on each slot for which the definition is set to be `:finalized`. This makes finalization of class slots automatic and convenient.

Since finalizables don't add any metaclass properties, there is no need to manually calculate inheritance order. However, as with all custom slot definitions, the slot properties must be copied over from the direct-slot instance to the effective slot. In the case of `finalizable-class` this happens in `compute-effective-slot-definition`.

In order to support finalizing of Qt class instances that don't have a CL class equivalent, the `finalize` method is extended wit ha `finalize-using-class` that is dispatched to using the Qt class and the instance of a `qobject` instance is passed to `finalize`.

### Widgets
As with finalizables, the widget is implemented using two classes, the `widget-class` metaclass and the `widget` superclass. These both inherit from the finalizable equivalents. The main crux of the `widget-class` lies in its `widget-class-direct-options` and `widget-class-extern-options`. The direct options are the options that are passed to a `re/` `initialize-instance` (and thus also to `defclass`). They're caught in the appropriately specialised methods and then stored on the class. The effect of this is that we can fully recompute the class definition at any time, and potentially add or remove options without influencing the original `defclass` statement. This is where the extern-options come in.

Using `set-widget-class-option` options can be added to the class definition dynamically at any point in the program. This function then adds the option to the class' extern-options and then calls `reinitialize-instance`, which in turn causes the class to get effectively redefined outside of its `defclass` form. This redefinition also allows us to change CommonQt class options. Using this we can create forms outside of the original `defclass` that act as if they were actually options in the `defclass` form.

Qtools effectively only provides two forms that do this: `define-signal` and `defmethod`. The `define-signal` is relatively straightforward and simply expands to a class option set to add a new signal option. The `defmethod` is an extensible machine in itself.

What's special about the `cl+qt:defmethod` is that it inspects the declaration forms in the method body. It then checks for each declaration form whether a handler function exist and if so, calls that function. Such a `method-declaration` function can then return forms to be put into the macroexpansion of the `cl+qt:defmethod`, before the resulting `cl:defmethod`. The processed declaration is then left out of the `cl:defmethod`form as it is assumed that it isn't a standard common lisp declaration. However, the declaration function also has the ability to change the contents of the `cl:defmethod` form itself, by manipulating `*method*`. This allows the declaration to output special handling for the method body, for example.

This kind of extensible declaration mechanism is necessary both to allow further evolving of Qtools in the future as well as adaptation by users. It also offers a very "native-like" way of specifying external effects of a method. Qtools uses this construct then to allow definition of slots, overrides, finalizers, and initializers.

In case the user doesn't appreciate the `defmethod` way, Qtools then provides `define-*` alternative functions that simply wrap over `defmethod`, establish some default bindings, and take care of naming and specialising the method.

The `define-subwidget` deserves special attention here, as it does more work than the rest. A large part of defining widgets is adding sub-components to it, which is a task that usually involves a lot of repetition or awkward function sharing: Setting up a slot to hold the instance, defining or using an initializer to set it up. `define-subwidget` makes this both distributed and simple by both taking care of setting up an appropriate initializer function, and automatically adding the slot to the class using, again, `set-widget-class-option`. This slot is also always automatically set to be `finalized` in order to ensure that all widgets are properly cleaned up when the GUI is no longer needed.

A minor problem regarding this approach is the same problem that appears with all of CL's definition forms. While developing incrementally, merely removing the definition form from the source file, will not actually remove it from the image. This can trip developers up, as definitions will still be active later. In this case it means having widgets still sticking around, or initializers running, etc. For this purpose there are corresponding `remove-*` functions to all the `define-*` functions to allow easy removal. This part cannot possibly be automated due to the nature of Common Lisp, but it is at least simple to correct should the need arise.

### Q+
CommonQt's way of dealing with method calls is the simple and most direct way of doing it. The first possible alternative to remove the need for typing method names in their corresponding case would be to simply introduce a different reader macro that automatically translates a `example-function`-like name into `exampleFunction` as is done already in other parts of Qtools. However, this has two downsides, the first being that there does not exist a 1:1 mapping of methods anywhere. The dynamic computation of the function name means that there isn't a full correspondence table anywhere. The second downside is that methods are still not passable as first-class objects.

The way to solve this is to generate actual CL wrapper functions to the method calls. This allows us to use them as first-class objects, have at least compile-time argument number checking, and have a linkage of wrapper name to Qt method by listing it in the docstring.

In order to achieve this, there are two possible choices. First, the entirety of all possible wrappers can be computed once, and then subsequently loaded into the image and used directly. However, this creates two new problems. Computing all wrappers, compiling them, and loading them, takes a long time and subsequently litters the image with thousands of functions and symbols that won't ever be used in the program. Then, we have a problem with the smoke modules, as we need to know which modules will be used in a potential application ahead of time, load them all, and then generate the wrappers. We cannot generate wrappers for each module separately, as the methods from different classes share the same wrapper functions. This means that whenever a different set of modules is needed, the wrappers need to be regenerated, recompiled, and reloaded. A lot of time and space goes to waste with this. However, this approach also has an advantage: As all functions are always available, it is easy to develop with. Arguments and docstrings will be readily available through the editor. Qtools offers this approach through `q+-compile-and-load`.

The second approach is to dynamically only compile what is needed. That way, the image only ever contains wrappers for function that are actually called (at some point). However, this complicates things a lot. When a function is compiled that calls such a function, it doesn't exist yet. Even worse, when the form is read, the symbol for the function does not exist yet and isn't external! In order to catch this problem, a modified reader macro is necessary. This reader macro will detect when a call to a wrapper function is made, and instead transform it into a macro call that then sees to it that the wrapper will be created. Modifying the reader in such a way is a heavy change, and should only be used sparingly, however there is no alternative here. Qtools does not force you to use this reader extension, you can always just use the macro directly.

However, dynamic compilation complications don't end there. Since we never dump the function to a file, it only ever exists in the environment it was compiled in. That means, if you compile a function that then dynamically generates the wrapper function, the wrapper won't be available anymore at load time. Qtools solves this issue with a trick. The `q+` macro expands to a `load-time-value` form that then generates the wrapper function. That way, the wrapper will always be available at load- and execution time, while posing no overhead to the execution time, as it will return a value to that won't impact anything.

Function referencing gets the same problems as function calling, so the Qtools readtable also contains an overridden `#'` reader macro to handle that. In the case of a wrapper call, it expands to `Q+FUN` which in turn expands to a `load-time-value` form that generates and returns the function object.

As is probably obvious by now, Qtools also implements the second approach. Therefore, the choice as a user is yours: You can statically precompile everything and use it directly, or you an use the dynamic on the fly compilation using either a readtable expansion, or a simple macro. The concepts to make this all possible are rather complex, but the actual function wrapper compilations are quite straight-forward. The system is currently not suited for extension, but I see no need to allow that as the kind of Qt methods that can exist are fixed and Q+ should handle all that are relevant.

Q+ does one last thing to fix the "issue" of having setters instead of being able to use `setf`. For this purpose it has an extended `setf` macro that checks if a place is a function in the Q+ package. if so, this place/value pair is instead expanded to a call to the appropriate wrapper function with the function name transformed. However, that's not the only reason to do this. The second is that some setters require multiple arguments to be set at once. Usually, `cl:setf` allows for cases like these by permitting setf expanders to accept multiple values. However, the number of values is fixed, and there's no way to dynamically know how many values where passed. Since Q+ needs to dispatch based on the number of values, this is not a viable approach. Therefore, with `cl+qt:setf` a `(values ..)` value form is specially treated and its arguments are inlined into the wrapper call. This also means that it isn't possible to use multiple values of a returning function as the values to a setter call, however while that is an inconsistency, I don't think it will be a big issue. If it turns out to be problematic in the later run, this will have to be changed to a dynamic analysis at run-time, which is an overhead I wanted to avoid.

One final note about the dynamic approach and its readtable: In order to recognise whether a call to a symbol in the Q+ package is made, it needs to read ahead multiple symbols and call `unread-char` multiple times consecutively, if it turns out that it is not a Q+ reference. Multiple consecutive calls to `unread-char` are not permitted by the hyperspec, so a different approach should be found eventually. Right now, this is an unresolved kludge and I will see about fixing it once it causes real problems.

## Support
Currently the following implementations are tested and supported by Qtools:

* [SBCL](http://www.sbcl.org/) (1.2.8 Lin64)
* [CCL](http://ccl.clozure.com/) (1.10 Lin64)

It may or may not work more or less smoothly on other implementations and platforms depending on MOP and CommonQt support and general implementation quirks.
