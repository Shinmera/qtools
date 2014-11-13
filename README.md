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

As you can see, it doesn't require string-escaping the Qt class name, as it can leverage `find-qt-class-name`. Further, it adds more class options: `:defsignals`, `:defslots`, `:defoverrides`, `:subwidget`, `:layout` and `:initializer`. You can get a detailed description of what each of them do with `describe-widget-option`. For example, the `:subwidget` option allows you to more easily define widgets within your main class:

    (define-widget my-widget (QWidget)
      ()
      (:subwidget 
        (button (#_new QPushButton "Hi!")
          (#_setFlat button T))))

Subwidgets are added to the class as a slot of the same name and are automatically finalized. All the special options that accept function bodies wrap them in `with-class-slots`, which means you'll always have easy access to your class properties by simply using their slot name.

### The Widget Environment
Since using class options is awkward for bigger functions, there's another macro: `with-widget-environment`. This allows you to 'outsource' the widget options into forms of their own, which then automatically get repackaged into the class definition. The above example thus becomes:

    (with-widget-environment
      (define-widget my-widget (QWidget) ())

      (define-subwidget button (#_new QPushButton "Hi!")
        (#_setFlat button T)))

The same works for all the other class options, with a similar `define-` form alias. You may however use any other form you normally would as well, it will be evaluated just as if it were put at the toplevel.

### Readtable
CommonQt provides a necessary readtable to add a convenient way to write foreign calls. Qtools provides its own named-readtable (`:qtools`) that inherits from this readtable, but adds some minor tweaks.

Currently, the reader macros `#<` to call `unbox` and `#>` to call `make-gc-finalized` on the following object are available.

## Extending Qtools
The Widget class options and the widget environment can be extended by the user. If you find yourself repeating certain actions or definitions, you may want to define shortcuts using these.

### Widget Options
Widget option expansion happens in two stages in order to account for the complications that arise with class initialisation. If your option needs to modify or add class slot options, you will have to use `define-widget-slot-option`. For expanding to qt-class options or other parts, rely on `define-widget-class-option`. When an option is evaluated it is done in the following manner:

    (defclass .. (:foo (..) (..)) (:bar ..)) => (:foo ((..) (..)) :bar (..)) => (call-slot-option :foo (..)) (call-slot-option :foo (..))...

Meaning the class options first get turned into a plist as per the way defclass works. Each 'body' of an option then gets individually passed to an option expander. Each call of an expander then returns a plist to use in place of the original (the plists get merged as per `fuse-plists`). This allows your option to expand to multiple different options without having to be aware of or risking disturbing other options.

For example, defining an option that takes a name and adds that as a slot would be something like the following:

    (define-widget-slot-option class-slot (class name)
      `(:direct-slots ((:name ,name :readers () :writers () :initargs ()))))

When writing slot expanders you need to be aware of the following complication: The class has not been initialised at the point the expander is evaluated, which means that none of the class' slots will be available and even the class' name might be undefined. You should really only use slot expanders to add slots and for everything else a class expander should be used. At that point the class should be properly initialised with its slots (as per `shared-initialize`).

### Environment Forms
Environment forms can be added with `define-environment-form`. As the primary value you should return a form to put in place of the original form (just like a macro would). The secondary value, if provided, should be a list of defclass options to pack into the `define-widget` form. For example you could do something like this:

    (define-environment-form docstring (string)
      (values NIL `((:documentation ,string))))

Which would then turn

    (with-widget-environment
      (define-widget foo () ()) 
      (docstring "foo"))

into

    (progn (define-widget foo () () (:documentation "foo")))

You can read more about what an environment form might do by using `describe-environment-form`.

### Copying and Finalizing
In order to account for your own objects and operations you can extend the `copy` and `finalize` functions by using `define-copy-method` and `define-finalize-method` or `defmethod` directly. The two define macros bring the convenience of automatically resolving to a Qt class (and thus using `copy/finalize-using-class`) if possible, making it all look a bit cleaner.

    (define-copy-method (instance QPixmap)
      "Creates a new QPixmap using QPixmap::copy (deep copy)."
      (#_copy instance (#_rect instance)))

Since copying and finalizing are operations associated with a certain amount of ambiguity, it is advisable to always write documentation strings for your `copy`/`finalize` methods. That way users can get a better idea of what will happen by reading about it using `describe-copy-method` and `describe-finalize-method` respectively.

## Debugging Qtools
Since Qtools does a bunch of contrived things, you might want to check what exactly is done if something doesn't go according to plan. I'm not excluding the possibility of bugs being around that mess your code up. In order to check this, you will want to load [verbose](http://shinmera.github.io/verbose/) before loading Qtools and set the logging level to trace: `(setf (v:repl-level) :trace)`. Qtools will emit log messages when you compile `define-widget` forms that contain the generated options. It will also log all objects that get passed to `finalize` and `copy`. Hopefully the log output will help you in discovering what's going on behind the scenes.

## Support
Currently the following implementations are tested and supported by Qtools:

* [SBCL](http://www.sbcl.org/) (1.2.4 Lin64)
* [CCL](http://ccl.clozure.com/) (1.10 Lin64)

It may or may not work more or less smoothly on other implementations and platforms depending on MOP and CommonQt support and general implementation quirks.
