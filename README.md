## About Qtools <a href="https://travis-ci.org/Shinmera/qtools"><img src="https://travis-ci.org/Shinmera/qtools.svg?branch=master" alt="Build Status" align="right" /></a>
Qtools is a collection of utilities to help development with CommonQt. There are a lot of things in there, from name mapping over garbage handling to widget class definition. Some tools are straightforward, others are quite complex. I'll try to explain everything as best I can.

## Fundamentals
There are three layers working beneath Qtools, of which you need to know about in order to understand how to use the various facilities Qtools offers.

### [Qt](http://qt-project.org/doc/qt-4.8/)
Qt is a gigantic toolkit that mainly concerns itself with GUI creation. It is cross-platform and written in C++. While its size and large user-base means that it is very mature and usable for pretty much any kind of GUI, the fact that it's written in C++ makes things complicated. The usual way to interoperate with C++ libraries and projects is to create a C API wrapper.

### [Smoke](https://techbase.kde.org/Development/Languages/Smoke)
And this is what Smoke does. It generates a C wrapper so that other languages can make use of Qt through C-FFI. Smoke is divided up into a bunch of modules, each being associated with a part of Qt, such as qtcore, qtgui, qtopengl, phonon, etc.

### [CommonQt](http://common-lisp.net/project/commonqt/)
The heavy lifting and ground-work that is required to interface with Smoke (and thus with Qt) is done by CommonQt. By itself, CommonQt offers a complete framework to create Qt GUIs from Common Lisp out.

### [Qtools](https://shinmera.github.io/qtools/)
Unfortunately, working with CommonQt itself is a bit awkward. While it offers everything you need, the way you have to make use of it is sub-par. Qtools attempts to help with this by offering another layer over CommonQt to smooth everything out. However, since you might not like certain parts of the Qtools ecosystem, it should be possible for you to only use the features you like, rather than being forced to use everything. So, you can always mix and match "plain" CommonQt and Qtools extensions.

## Getting Started
Before getting started with explaining the details on the various parts of Qtools, I'll go through a basic project setup of a project using it.

First you will want an ASDF system to define your project. In its dependencies it should contain `:qtools` and the smoke modules you require, usually just `:qtcore` and `:qtgui`.

    (asdf:defsystem qtools-intro
      ...
      :depends-on (:qtools :qtcore :qtgui))

Then of course you'll usually want a package to put all your stuff in. Instead of `:use #:cl` you will most likely want to `:use #:cl+qt`. This package combines the symbols of the common-lisp base package, the commonqt package, and qtools. Thus all the symbols and functions you usually need for development are already included.

    (defpackage #:qtools-intro
      (:use #:cl+qt)
      (:export #:main))

CommonQt, and Qtools itself, require a few extensions to the standard reader syntax. For this reason you will want to change the readtable using `named-readtables:in-readtable`, which `cl+qt` includes. Readtable changes are on a *per-file* basis, so you need both an `in-package` and an `in-readtable` call on every file.

    (in-package #:qtools-intro)
    (in-readtable :qtools)

This sets up everything you need to get started writing an actual GUI. So let's do that as well. In Qt things are organised as widgets. Windows are widgets, buttons are widgets, text fields are widgets, etc. Qtools mirrors this.

    (define-widget main-window (QWidget)
      ())

This `define-widget` form is syntactically equivalent to `defclass`. The only change is that the first argument in the superclass list is the Qt class to inherit from. Now we'll want to add some things to display in the widget.

    (define-subwidget (main-window name) (q+:make-qlineedit main-window)
      (setf (q+:placeholder-text name) "Your name please."))

    (define-subwidget (main-window go) (q+:make-qpushbutton "Go!" main-window))

This adds a QLineEdit widget called `name` to the `main-window` and sets its placeholder text to `"Your name please."`. The second form adds a button called `go` with a label of `"Go!"`. Simple stuff. The body of the `define-subwidget` form can contain any number of statements. By default, the symbols of all the other subwidgets and slots defined prior to the `define-subwidget` form are bound to their corresponding values. This is useful if you for example need to define a layout, as we will do now.

    (define-subwidget (main-window layout) (q+:make-qhboxlayout main-window)
      (q+:add-widget layout name)
      (q+:add-widget layout go))

This sets up the displaying part of our GUI, but so far we haven't made it react to anything yet. Reacting to events in Qt happens through signals and slots. Slots are functions that receive signals, and signals are event carriers.

    (define-signal (main-window name-set) (string))

    (define-slot (main-window go) ()
      (declare (connected go (pressed)))
      (declare (connected name (return-pressed)))
      (signal! main-window (name-set string) (q+:text name)))

    (define-slot (main-window name-set) ((new-name string))
      (declare (connected main-window (name-set string)))
      (q+:qmessagebox-information main-window "Greetings" (format NIL "Good day to you, ~a!" new-name)))

We're doing things a bit roundabout here to illustrate creating signals. `define-signal` introduces a new signal called `name-set` that takes a single `string` as argument. We then define a new slot that is connected to the `go` button's `pressed` signal (which has no arguments) as well as the `name` field's `return-pressed`. We then simply fetch the current text of our `name` field and send it out again with our custom signal. The second slot catches this signal again and uses it to display a message box.

And that's that. The only thing we didn't take a look at here is `define-override`, which allows you to define override functions for your Qt classes. So, if you for example want to manually draw onto a widget you can override its `paintEvent` method using this.

    (define-override (main-window paint-event) (event)
      (declare (ignore event))
      (with-finalizing ((painter (q+:make-qpainter main-window)))
        (q+:fill-rect painter (q+:rect main-window) (q+:qt.white))))

This'll make the background of our window completely white. The important thing to note here is the `with-finalizing`. What has been rather well hidden from you so far, is that as Qt is a C++ framework, you will have to do manual memory management. Qtools makes this a lot easier by offering macros and automations to take most of it off of your hands. For example, all the sub widgets we defined are automatically deleted once the main window is.

A general note about developing with Qtools/CommonQt: While custom function bodies such as from qt-slots, overrides, initializers, and finalizers reside on the CL side and can thus be redefined at any time and take effect immediately, adding or removing qt-slots and overrides will not affect already created instances. This is to say, if you run your application and recompile your override, the effect will be visible immediately. But if you add a new slot, override, or signal, the existing instance will not have them. This is due to the fact that these things need to be tied to the C++ class, which will not update existing instances when it is changed, like CLOS usually does. This means that if you change a running widget by adding new components, you need to recreate or restart it to see the effects.

## Qtools Components

### Name Conversion
Common Lisp and C++ follow different naming conventions for classes, variables, methods, and types. Qtools offers a couple of functions to attempt to translate between the two. Finding a Qt class can be done with `find-qt-class-name`. It searches a static table of known Qt classes (`*qt-class-map*`) by first stripping all dashes from the name, and then case-insensitively finding a matching name.

Translating method names can be done through `to-method-name`, which translates dashes to mean that the next character should be in uppercase. So `foo-bar` is `fooBar`. In case you pass it a string instead of a symbol however, it will not do any translation. This is important to take care of edge-cases, where this primitive translation would prohibit using a certain name. If you need a method signature instead of just a name, there are `specified-type-method-name` that takes a name and a list of type specifiers, and `determined-type-method-name`, which attempts to determine the type of the arguments, rather than requiring type specifiers directly.

Types are translated using `qt-type-of`, `eqt-type-of`, `qt-type-for`, and `to-type-name`. Where `qt-type-for` translates a type specifier and `qt-type-of` tries to determine the type through a value. Reversing from a Qt type specifier to a CL type is possible with `cl-type-for` and `ecl-type-for`.

### Object Handling
In C++ there usually is no garbage collector, so you need to carefully clean up yourself or you'll create a memory leak. In Common Lisp we're used to the luxury of not having to worry about this, thanks to garbage collection. Sadly, we cannot use the garbage collector to also take care of C++ objects, as they live in a different world not governed by us. So, garbage collection of Qt objects is still our own worry.

To make this all just a smidgeon easier, Qtools introduces a system of *finalizables*, the central point of which is the `finalize` generic function. This function should take care of all necessary cleanup when an object is no longer needed. For `qobject`s, this means running eventual cleanup (through `finalize-using-class`) and then being `delete`d, thus properly removed from memory. However, `finalize` can not only be used for Qt objects, but for anything else as well. Especially interesting here are `finalizable` objects, whose slots can specify an additional argument `:finalized`, which dictates whether `finalize` is run on the slot's value when the class instance is `finalize`d.

Often times we only need Qt instances for a certain lexical context of code. For this case, `with-finalizing` (and `with-finalizing*`) can be used, which is a counterpart to `let` that calls `finalize` on all its bindings once the form exits. This will take care of most cases. For cases where the instance may escape, or has to stay bound in a closure, there's `with-gc-finalized`. This wraps the value of each binding in a `gc-finalized` object. Using such a container, we can use Common Lisp's garbage collector to keep track of the references and, once the object is garbage collected it calls `finalize` on its inner value, ensuring proper removal. The `with-gc-finalized` uses a `symbol-macrolet` to ensure that you don't have to worry about the boxing. However, if you manually use `gc-finalized` objects, there's the `make-gc-finalized` and `unbox` functions (with corresponding reader macros `#>` and `#<` in the `:qtools` read-table).

While this kind of system takes care of a lot of cases, it's still not perfect and it may happen that you accidentally create a memory leak somewhere. I wish there was an easier way, but alas, life is difficult. You can try to solve this kind of situation by debugging Qtools and keeping track of which objects get finalized and which don't.

Another occasional task is to copy an object instance. Qtools offers a `copy` and `copy-using-class` methods which handle proper copying for a couple of Qt objects, but sadly by far not all. If you want to use the copying system for a class that isn't handled by Qtools by default, you can define your own using `define-copy-method`.

### Widgets
Qt deals with widgets. As such, making everything associated with them simple and easy to use should be a primary objective. Qtools' widget system attempts to do exactly that. The central part to this is the `define-widget` macro. This expands to a `defclass` with the following effects: It sets `widget-class` as the metaclass, sets the first item of the superclass list as the qt-superclass, and injects `widget` as a superclass, if it isn't one already. This means that essentially you can use anything you could in a standard `defclass` without having to worry about the necessary default options.

However, just with a single `define-widget` you won't get far ahead. You still need to use CommonQt's way of declaring slots, signals, and overrides, which is quite cumbersome. You can do that of course, but there is a more convenient method, which is to use `define-slot`, `define-signal`, and `define-override`. These essentially translate to class options, albeit in a detached way. Each of these define statements is of the following syntax:

    (define-* (widget-class name) arglist &body body)

Some of them take optional extra arguments in the name-list, such as a method-name in the case of `define-slot` and `define-override`. In the case of `define-signal` the body is discarded. However, even with these extensions things are rather cumbersome: You need to manually define slots for each of the widgets you want to use inside your widget class, and define their behaviour in an `initialize-instance` function. This is unwieldy, which is why Qtools also adds `define-initializer`, `define-finalizer`, and `define-subwidget`. The first two follow in signature to the above minus the `arglist` and do what you might expect them to do: handle initialization and finalization. The initializer and finalizer forms take an optional priority argument in their name-list. The higher, the sooner. `define-subwidget` on the other hand looks like this:

    (define-subwidget (widget-class name) initform &body body)

And its effects are two-fold. First, it adds a finalized slot to the widget-class called name. Then, it adds an initializer (with priority 10) that sets the slot-value to the value of `initform` and then evaluates the `body` forms. This by itself takes care of the repetitive slot and initializer definition, but without an extra helper called `with-slots-bound`, it would still be annoying to write functions, as you would have to reference widgets using `slot-value` everywhere. `with-slots-bound` is like `with-slots`, but it binds *all* direct class-slot values to their respective slot names. Every `define-*` function's body is automatically wrapped in a `with-slots-bound`, to make this convenience possible.

If you do not like this behaviour, due to potential symbol clashes and general confusion that might arise from the implicit action, you can instead use `cl+qt:defmethod` and `declare` statements. This is actually what all the `define-*` (with the exception of `define-signal`) expand to: A `cl+qt:defmethod` with an appropriate declaration inserted into the body. The `cl+qt:defmethod` behaves exaclty like `cl:defmethod` with the exception of allowing the handling of custom declaration forms. Qtools defines the following declarations: `slot`, `override`, `initializer`, and `finalizer`. In the case of a `slot` definition, an extra declaration called `connected` is also available. The effects of the declarations are as you might expect, and have the following signatures:

    (slot slot-name args)
    (connected slot-name (signal-name &rest args))
    (override &optional method-name)
    (initializer &optional (priority 0))
    (finalizer &optional (priority 0))

The user may define additional declarations using `define-method-declaration`.

At this point it is useful to note about the general startup sequence of a widget. One `make-instance` is called, it calls to `initialize-instance`. Qtools defines a primary method on this specialized on the widget class. It then immediately calls the next method (`call-next-method`), followed by `qt:new` which instantiates the C++ parts. Following that are the initializers in the order of their priority (highest goes first). This means that if you (for some reason) define an `:after` method on `initialize-instance` for your own widget, it will be run after all initializers have completed. For finalization it is the same: all finalizers are run before your own `finalize` method is run. However, if you define a `:before` method on `finalize` for your class, it will be run before the `:finalized` slots are finalized, otherwise all primary and `:after` methods should not access `:finalized` slots anymore. If you absolutely do need to do things before normal initialization or finalization, you can define an `:around` method.

One last widget-related definition form is `define-menu`, which is a very convenient way of specifying menus:

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

Out of the box, it supports `:item`, `:menu`, and `:separator` components. The item takes a name, which can be either a string for a label, a list of string and keyboard mnemonic, or a symbol indicating the class slot that contains the item widget, and it takes a body of forms to execute if the item is triggered. Menus take a name as a string and a body of components to contain, or a symbol indicating the slot that contains the menu widget. New components can be added with `define-menu-content-type`.

As a final touch, Qtools offers macros for connecting slots and emitting signals. These translate to CommonQt's `emit-signal` and `connect` functions, and thus just offer a bit of syntactic sugar. They're called `signal!` and `connect!`. Their use is simple enough:

     (connect! input (text-edited string) widget (text-changed string))
     (signal! input (text-edited string) "Eyyyy")

An experimental variant for the adventurous is `generic-signal`, which attempts to determine the argument types by their values at run-time. It does therefore not require specifying the type explicitly, but might instead screw up and choose a wrong type for the signal and thus fail to emit.

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

In order to access enum values, you simply use the class name followed by a dot and the enum name. Constructors are the class name prefixed with "make". Static functions are the class name, a dash, and the method name in the standard translation scheme.

    (q+:qt.blue)
    (q+:make-qpushbutton "Foo!")
    (q+:qmessagebox-information parent "!" "hello!")

For the specific arguments, names, and everything else, refer to the [Qt4.8](http://qt-project.org/doc/qt-4.8/) documentation. It's very good, trust me. The only thing you need to be aware of is the name conversion rules that Q+ uses to determine the proper Lisp symbol to use:

* Method `QImage::constScanLine` => `q+:const-scan-line`
* Method `Foo::set_widget` => `q+:set_widget`
* Constant `QImage::InvertRgb` => `q+:qimage.invert-rgb`
* Constant `QImage::Format_ARGB32_Premultiplied` => `q+:qimage.format_argb32_premultiplied`
* Static Method `QFileDialog::getExistingDirectory` => `q+:qfiledialog-get-existing-directory`
* Constructor `QImage::QImage` => `q+:make-qimage`

For Q+ to work seamlessly in conjunction with ASDF systems and compiling/loading code, you have to make sure that the smoke modules are set up correctly.

`q+` and the reader extension dynamically compile wrapper functions for the Qt methods you access. You can, however, also precompile all possible methods for the currently active set of smoke modules. To do this, you can either `:depends-on (:q+)` or compile a source file using `write-everything-to-file` and include it in your ASDF system. If you choose this approach, you will not need to switch the readtable or use the `q+` macro, as the package will be available fully populated.

### Smoke Modules
In order to be able to use the various parts of Qt, the corresponding smoke modules need to be loaded. By default CommonQt loads `qtcore` and `qtgui` when `make-qapplication` is called. However, if you want to use, say, the OpenGL parts you'll also need `qtopengl`.

Qtools provides ASDF systems for all the different smoke modules. That way, you can simply push the modules you want into your project's ASDF system dependencies and it'll ensure that the modules are available at compile and load time. Having the modules loaded at both times is especially important for Q+ to work properly. An example system making use of this would look like

    (asdf:defsystem foo
      ...
      :depends-on (:qtcore :qtgui))

For a list of available smoke modules, see `*smoke-modules*`.

## Examples
A couple of example applications using Qtools can be found in the [examples/](https://github.com/Shinmera/qtools/tree/master/examples/) folder: `qtools-evaluator`, `qtools-titter`, `qtools-melody`, and `qtools-opengl`. Each of them can be loaded by their name, and launched using the `main` function from their package.

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

In this example we're using [form-fiddle](http://github.com/Shinmera/form-fiddle) to parse the method form. Using a library like that to ensure proper destructuring is important, as otherwise it's easy to accidentally butcher the method form, or get the wrong information.

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
