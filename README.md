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

Currently, the reader macros `#<` to call `unbox` and `#>` to call `make-gc-finalized` on the following object are available.

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
The menu definition form allows for arbitrary content types, so you may add new ones yourself by using `define-menu-content-type`. This function will be called during the initialisation sequence of the widget, with the widget instance bound to `*widget*`. If your menu option requires modification of the class instance of some sort, you can take advantage of `set-widget-class-option` and `softly-redefine-widget-class`. In case the option should be able to contain sub-forms like a `:menu` does, you can process further forms by using `build-menu-content`.

## Debugging Qtools
Since Qtools does a bunch of contrived things, you might want to check what exactly is done if something doesn't go according to plan. I'm not excluding the possibility of bugs being around that mess your code up. In order to check this, you will want to load [verbose](http://shinmera.github.io/verbose/) before loading Qtools and set the logging level to trace: `(setf (v:repl-level) :trace)`. Qtools will emit log messages when you compile `define-widget` forms that contain the generated options. It will also log all objects that get passed to `finalize` and `copy`. Hopefully the log output will help you in discovering what's going on behind the scenes.

## Support
Currently the following implementations are tested and supported by Qtools:

* [SBCL](http://www.sbcl.org/) (1.2.4 Lin64)
* [CCL](http://ccl.clozure.com/) (1.10 Lin64)

It may or may not work more or less smoothly on other implementations and platforms depending on MOP and CommonQt support and general implementation quirks.
