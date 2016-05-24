#|
 This file is a part of Qtools
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

;; class-map.lisp
(docs:define-docs
  (variable *qt-class-vector*
    "A vector of all Qt4.8 class names as strings.")
  
  (variable *qt-class-map*
    "An EQUALP hash-table of all Qt4.8 class names to themselves as strings.")

  (function find-qt-class-name
    "Returns the string designating an equivalent Qt class. You can use this to resolve
symbols and 'lisp-ified' names to Qt class names. Hyphens are stripped from the designator.

See *QT-CLASS-MAP*")

  (function eqt-class-name
    "Returns the string designating an equivalent Qt class, if possible.
If the designator is a string, it is returned immediately without further check.
If it is a symbol, it is resolved through FIND-QT-CLASS-NAME, and if no name can
be found through that, an error is signalled."))

;; copying.lisp
(docs:define-docs
  (function copy
    "Generates a copy of the object.

The way objects are copied varies, but usually it can be assumed that the
copy is made in a way such that data immediately associated with the object
is copied (such as pixel data in an image), but data only implicitly
referenced (such as the paint device of a painter) is not.

Use DESCRIBE-COPY-METHOD for information on a specific copying mechanism.

Uses COPY-QOBJECT-USING-CLASS and determines the class by QT::QOBJECT-CLASS.")

  (function define-copy-method
    "Defines a method to copy an object of CLASS.
CLASS can be either a common-lisp class type or a Qt class name.

Qt class names will take precedence, meaning that if CLASS resolves
to a name using FIND-QT-CLASS-NAME a QCLASS-COPY method
is defined on the respective qt-class. Otherwise a COPY method
is defined with the CLASS directly as specializer for the instance.

In cases where you need to define a method on a same-named CL class,
directly use DEFMETHOD on COPY-QOBJECT.

See COPY-QOBJECT")

  (function describe-copy-method
    "Prints information about the copy method for the specified class if possible."))

;; deploy.lisp
(docs:define-docs
  (variable *foreign-libraries-to-reload*
    "A list of CFFI libraries that need to be reloaded on boot.

The system sets this variable itself during the build.")

  (variable *smoke-modules-to-reload*
    "A list of smoke modules that need to be reloaded on boot.

The system sets this variable itself during the build.")

  (variable *build-hooks*
    "A list of functions to invoke (in sequence) during the build.

Use this to run customised cleanup, compile, or deployment functions.")

  (variable *boot-hooks*
    "A list of functions to invoke (in sequence) during the warm-boot.

Use this to run customised startup, prepare, or load functions.")

  (variable *quit-hooks*
    "A list of functions to invoke (in sequence) when the program quits.

Use this to run customised saving or cleanup functions.")
  
  (function build-qt-system
    "Shorthand for `(operate 'asdf:qt-program-op system)`. See OPERATE for details.")

  (function user-libs
    "Accessor to the list of user libraries defined under the given name.")

  (function remove-user-libs
    "Remove the user libs defined under the given name.")

  (function define-user-libs
    "Allows you to define custom user libraries to hook into the deployment system.

If you have external libraries that are not provided by Qtools directly, you
need to tell the system where to find them in order for it to be able to copy
and load them properly during deployment.

The basic layout of the macro is as follows:

\(define-user-libs (my-libs #p\"~/my-libs/\" #p\"/some/custom/path\")
  (cffi-lib-name)
  (#p\"some_lib.so\"))

This defines two new libraries under the MY-LIBS label. It tells the system
that for both of them it should look in ~/my-libs and /some/custom/path.
The first library is the CFFI library named CFFI-LIB-NAME. The second library
is specified through an explicit pathname.

If the system can find the libraries in the specified locations, it will copy
them to the deployment directory and automatically load them if they were
loaded at the time of dumping (which is usually the case for CFFI).

See USER-LIBS
See REMOVE-USER-LIBS
See DISCOVER-FOREIGN-LIBRARY-LOCATION")

  (function discover-foreign-library-location
    "Attempts to discover a path for the given library and search-paths.

Aside from the explicitly passed search-paths, the following paths are
automatically searched:
  
  CFFI:*FOREIGN-LIBRARY-DIRECTORIES*
On UNIX:
  LD_LIBRARY_PATH
  /lib
  /usr/lib
  /usr/local/lib
On DARWIN:
  DYLD_LIBRARY_PATH
On WINDOWS:
  PATH
  C:/Windows
  C:/Windows/System32
  C:/Windows/SysWOW64

In order to avoid relisting big directories over and over during deployment
the results of a directory listing are cached in *FOLDER-LISTING-CACHE*.
A file matches if it matches the specified lib in both PATHNAME-NAME and
PATHNAME-TYPE by EQUAL comparison.")

  (function clean-symbol
    "Removes the docstring from the symbol if possible.")

  (function prune-symbol
    "Detaches function and variable binding from the symbol.")

  (function prune-package
    "Runs CLEAN/PRUNE-SYMBOL on each symbol home to the package and then deletes the package.")

  (function prune-local-paths
    "Attempts to remove references to local system paths that you probably don't want to leak into your image.

Affects CFFI:*FOREIGN-LIBRARY-DIRECTORIES*
        QT-LIBS:*STANDALONE-LIBS-DIR*
        *FOLDER-LISTING-CACHE*
        *USER-LIBS*")

  (function prune-foreign-libraries
    "Closes all opened foreign libraries that CFFI knows about.")

  (function prune-image
    "Attempts to clean up the image somewhat.

1. Runs CLEAN-SYMBOL on all symbols
2. Sets the *QAPPLICATION* to NIL
3. Calls QT::RELOAD to prune the smoke module state
4. Runs PRUNE-LOCAL-PATHS
5. Runs PRUNE-FOREIGN-LIBRARIES")

  (function smoke-library-p
    "Attempts to determine whether the given CFFI lib is a smoke library.")

  (function smoke-required-libs
    "Returns a list of required system (qt-libs) libraries.")

  (function deploy-foreign-libraries
    "Copies the given libraries over to the target directory.")

  (function boot-foreign-libraries
    "Loads all the libraries from *FOREIGN-LIBRARIES-TO-LOAD*

See *FOREIGN-LIRBARIES-TO-LOAD*")

  (function warmly-boot
    "Boots up the image to ready it for further execution.

0. If VERBOSE was present, its global controller is restarted
1. Sets the QT-LIBS:*STANDALONE-LIBS-DIR* to the UIOP:ARGV0 pathname
2. Runs QT-LIBS:LOAD-LIBCOMMONQT with FORCE
3. Reloads all smoke modules from *SMOKE-MODULES-TO-RELOAD*
4. Runs PROCESS-ALL-METHODS to ready Q+
5. Calls all *BOOT-HOOKS*")

  (function quit
    "Cleans up the image to ready it for a clean quit.

1. Calls all *QUIT-HOOKS*
2. Deletes the *QAPPLICATION*
3. Runs UIOP:FINISH-OUTPUTS
4. Exits the image")

  (function call-entry-prepared
    "Wraps the ENTRY-POINT function in a proper startup/shutdown sequence.

1. Establishes an EXIT restart which calls QUIT
2. Runs WARMLY-BOOT
3. Calls the entry-point
4. Invokes the EXIT restart.")

  (function discover-entry-point
    "Attempts to discover a suitable entry point for the given ASDF component.

This looks at the ASDF/SYSTEM:COMPONENT-ENTRY-POINT value and attempts to 
coerce it into a class or a function. If it is a class, then the entry point
will be (lambda () (with-main-window (window (make-instance entry-point)))),
if it is a function it will just be called verbatim.")

  (type qt-program-op
    "An ASDF:PROGRAM-OP subclass to handle things for Qt deployment.

You should specify this as the BUILD-OPERATION in your ASD along with
an ENTRY-POINT and a BUILD-PATHNAME.")

  (function compute-libraries-to-reload
    "Attempts to compute a list of all libraries that need to be reloaded on boot."))

;; dispatch.lisp
(docs:define-docs
  (function qclass-class-list
    "Returns a list containing the given class and all of its transient superclasses.")

  (function qclass-precedence-set
    "Builds a class precedence set for the class and all of its transient superclasses.")

  (function direct-qsubclass-p
    "Returns T if MAYBE-SUPERCLASS is a direct superclass of QCLASS.")

  (function compute-qclass-precedence-list
    "Calculates the class precedence list for the given qclass as per CLHS 4.3.5")

  (variable *qclass-precedence-lists*
    "Holds a map of computed precedence lists for qclasses.

We can cache this since the Qt class hierarchy is static.")

  (function qclass-precedence-list
    "Returns the class precedence list for the given qclass.

See *QCLASS-PRECEDENCE-LISTS*
See COMPUTE-QCLASS-PRECEDENCE-LIST")

  (function *qclass-precedence-list*
    "Holds the current class precedence list according to which is being dispatched.")

  (function dispatch-by-qclass
    "Dispatches on the given-method locator by the object.

The METHOD-LOCATOR should be a function of a single argument-- a qclass,
which returns the appropriate method for that class or NIL if none.
If the method-locator does not return a function for all classes in the
precedence list for the object, NO-APPLICABLE-METHOD is called.

This binds *QCLASS-PRECEDENCE-LIST*.

See QCLASS-PRECEDENCE-LIST")

  (function generate-qclass-dispatch-lambda
    "Generates a named-lambda form suitable for usage in a qclass-method.

Specifically, it establishes the appropriate local NEXT-METHOD-P and
CALL-NEXT-METHOD functions within the lambda body. Both of them behave
exactly like the CLOS ones do-- NEXT-METHOD-P returns T if there is a
next method that can be dispatched to, and CALL-NEXT-METHOD calls this
method if it exists or calls NO-NEXT-METHOD if it does not exist.")

  (function qinstancep
    "Tests whether INSTANCE is an INSTANCE of CLASS.

This includes subclasses, so a QSlider instance is also
an instance of a QWidget and so forth.")

  (function define-qclass-dispatch-function
    "Defines a sort of generic function that dispatches by qclass.

This can be used to write methods that dispatch as CLOS would, but on Qt
internal classes. In specific, it defines the following things all in one:

1. A variable *QCLASS-basename-FUNCTIONS* that contains all the methods you
   define on the function.
2. An accessor function QCLASS-basename-FUNCTION that takes a qclass and
   accesses the appropriate method function.
3. A function REMOVE-QCLASS-basename-FUNCTION to remove a method function.
4. A macro DEFINE-QCLASS-basename-FUNCTION to define a method on the
   qclass generic function. The macro will expect a qclass, a lambda-list,
   and a body as arguments.
5. A function DISPATCHER that is used as the \"generic function\".

Note that multiple-dispatch is not possible with this. Dispatch only ever
happens on the first argument, which must be a qclass instance. There is
also no method combination. Defining a second method on the same qclass will
simply replace the old definition.

However the local CALL-NEXT-METHOD and NEXT-METHOD-P functions are available
in a method body.

See GENERATE-QCLASS-DISPATCH-LAMBDA
See DISPATCH-BY-QCLASS"))

;; dynamic.lisp
(docs:define-docs
  (function to-readtable-case
    "Translates STRING to the proper reading case according to READTABLE.

See CL:READTABLE-CASE")

  (function ensure-q+-method
    "Ensures that the Q+ FUNCTION exists by compiling it on the fly.
Raises an error if no appropriate function can be found.
Returns the proper *TARGET-PACKAGE* symbol for the function.

See QTOOLS:ENSURE-METHODS-PROCESSED
See QTOOLS:COMPILE-WRAPPER
See QTOOLS:*TARGET-PACKAGE*")

  (function q+
    "Emits a function call to the Q+ FUNCTION with ARGS.

This macro does a bit of a complicated thing:
Firstly, it calls ENSURE-Q+-METHOD on FUNCTION to
make sure that the function object exists at compile
time. Then it emits a PROGN form that contains two
forms, the first of which is a LOAD-TIME-VALUE form
with a call to ENSURE-Q+-METHOD again. This is required
since the function compiled by ENSURE-Q+-METHOD is not
dumped to file anywhere and thus must be recreated at
load time to be available. The second form in the PROGN
is the actual function call, using the proper symbol
from the *TARGET-PACKAGE*.

See QTOOLS:ENSURE-Q+-METHOD")

  (function q+fun
    "Emits a form that evaluates to the function object of FUNCTION.

Specifically, it returns a LOAD-TIME-VALUE form that evaluates to
the function object, while ensuring that the function does indeed
exist.

See QTOOLS:ENSURE-Q+-METHOD")

  (function process-q+-setter
    "Processes a PLACE and VALUE pair for a Q+ setter call.
PLACE should be a form calling the Q+ macro, or a form calling
a symbol in the *TARGET-PACKAGE* directly. The name of the
function being called is prefixed with \"SET-\", and is then
used to form the function name for the resulting Q+ call.
If the VALUE is a VALUES form, then all the parts of values
are used as individual arguments in the resulting Q+ call.

Example: (process-q+-setter '(q+ foo 0 1) '(values 2 3))
=> (q+ \"FOO\" 0 1 2 3)

See QTOOLS:Q+
See CL+QT:SETF
See QTOOLS:*TARGET-PACKAGE*")

  (function cl+qt:setf
    "A wrapper around CL:SETF that specially handles calls to Q+ functions.

If a place is a Q+ form, or a form calling a symbol from *TARGET-PACKAGE*,
it is translated to a proper setter call using PROCESS-Q+-SETTER. Any other
place and value pair is translated to a normal CL:SETF call.
The order of places and values is preserved.

See QTOOLS:Q+
See QTOOLS:PROCESS-Q+-SETTER
See QTOOLS:*TARGET-PACKAGE*")

  (function fsetf
    "Finalizing SETF. The same as CL+QT:SETF, but performs a FINALIZE on the place first.
The finalize is performed before the place is set, but after the new value is evaluated.")

  (function cl+qt:defgeneric
    "Defines a new generic function.

Identical to CL:DEFGENERIC, but takes care of translating
function-names with SETF to use CL:SETF instead of CL+QT:SETF.

See CL:DEFGENERIC.")

  (function cl+qt:function
    "Defines a new function.

Identical to CL:DEFUN, but takes care of translating function-names
with SETF to use CL:SETF instead of CL+QT:SETF.

See CL:DEFUN.")

  (function cl+qt:fdefinition
    "Accesses the current global function definition named by NAME.

Identical to CL:FDEFINITION, but takes care of translating function-names
with SETF to use CL:SETF instead of CL+QT:SETF.

See CL:FDEFINITION."))

;; finalizable.lisp
(docs:define-docs
  (type finalizable-class
    "Metaclass for classes with finalizable slots.")

  (type finalizable-slot
    "Superclass for slots with a finalized option.")

  (type finalizable
    "A class for finalizable objects.")

  (function define-finalizable
    "Shorthand around DEFCLASS to create a finalizable class.

Automatically adds FINALIZABLE as direct-superclass and 
FINALIZABLE-CLASS as metaclass.")

  (function finalize
    "Finalizes the object. The effects thereof may vary and even result in nothing at all.
After FINALIZE has been called on an object, it should not be attempted to be used in any fashion
whatsoever as it may have been rendered unusable or unstable.

This method should be called on any object once it is known that it can be discarded.
FINALIZE will then try to clean up objects and make sure that they don't clutter your
memory, as lingering QOBJECTs would.")

  (function define-finalize-method
    "Defines a method to finalize an object of CLASS.
CLASS can be either a common-lisp class type or a Qt class name.

Qt class names will take precedence, meaning that if CLASS resolves
to a name using FIND-QT-CLASS-NAME a FINALIZE-QCLASS method
is defined on the respective qt-class. Otherwise a FINALIZE method
is defined with the CLASS directly as specializer for the instance.

In cases where you need to define a method on a same-named CL class,
directly use DEFMETHOD on FINALIZE.

See FINALIZE")

  (function describe-finalize-method
    "Prints information about the finalize method for the given class if possible.")

  (function with-finalizing
    "Executes the body as by LET and calls FINALIZE on all the objects introduced by
the bindings on completion of the body. If an error occurs during the binding phase,
all objects bound up until that point are still finalized. Finalizing happens in
reverse order of the bindings specified.")

  (function with-finalizing*
    "Executes the body as by LET* and calls FINALIZE on all the objects introduced by
the bindings on completion of the body. If an error occurs during the binding phase,
all objects bound up until that point are still finalized. Finalizing happens in
reverse order of the bindings specified."))

;; gc-finalized.lisp
(docs:define-docs
  (type gc-finalized
    "Wrapper object to allow automatic calling of FINALIZE by the GC.
Since you cannot finalize the object that is GC-ed itself, we need to wrap our to-
be-finalized object in another object that takes all the references instead.

This means that if you wish your object to remain unfinalized, you need to retain
references to the wrapper. As soon as the wrapper is hit by the GC, FINALIZE is
called on the object it contains.

In order to retrieve the contained object, use UNBOX.")

  (function make-gc-finalized
    "Wrap the OBJECT in a GC-FINALIZED instance. Use UNBOX to retrieve the object again.")

  (function with-gc-finalized
    "Creates bindings as per LET with the special note that each value of a binding is wrapped
in a GC-FINALIZED. Each bound symbol is shadowed by a SYMBOL-MACROLET, which evaluates to
the bound value as per UNBOX.

In other words, this will look like a standard LET to you, but each value of the let is
automatically ensured to be GC-ed and FINALIZEd once the body exits."))

;; generate.lisp
(docs:define-docs
  (variable *target-package*
    "The package used to store Qt wrapper functions that the Q+ system uses.
By default this package is called \"Q+\". The package should not contain
any systems except for those generated by Qtools.")

  (variable *smoke-modules*
    "A list of all possible smoke modules.

These modules provide the C wrappers required to work with
the respective Qt parts. Usually you will only need
QTCORE and QTGUI, but for example if you need OpenGL support
you'll want QTOPENGL, or if you need phonon, you'll want
the PHONON module.")

  (variable *operator-map*
    "A hash-table of C++ operators to CL function names.")

  (variable *qmethods*
    "Table mapping a *TARGET-PACKAGE* symbol to a list of
associated Qt methods. This table should only be changed
by PROCESS-METHOD. If you modify yourself without knowing
exactly what you're doing you'll most likely run into problems.

Methods/functions contained in this table are available
for compilation.

See QTOOLS:PROCESS-METHOD
See QTOOLS:COMPILE-WRAPPER")

  (variable *generated-modules*
    "A list of loaded smoke modules when PROCESS-ALL-METHODS is called.
This is useful to keep track over environments which modules are
actually available for compilation.")

  (function load-all-smoke-modules
    "Loads all the smoke modules as passed.

See QT:ENSURE-SMOKE")

  (function loaded-smoke-modules
    "Returns a fresh list of currently loaded smoke modules.

See QTOOLS:*SMOKE-MODULES*
See QT:NAMED-MODULE-NUMBER")

  (function clear-method-info
    "Clears the *QMETHODS* table.

See QTOOLS:*QMETHODS*")

  (function string-starts-with-p
    "Returns T if the STRING starts with START.

Strings are compared using STRING=")

  (function qmethod-operator-p
    "Returns T if the METHOD is an operator.")

  (function qmethod-cast-operator-p
    "Returns T if the METHOD is a casting operator.")

  (function qmethod-bogus-p
    "Returns T if the METHOD is bogus and unneeded.")

  (function qmethod-translatable-operator-p
    "Returns T if the operator METHOD is one that can be compiled by Qtools.")

  (function qmethod-globalspace-p
    "Returns T if the method is from the QGlobalSpace class.")

  (function clean-method-name
    "Returns a cleaned up name of METHOD.
This effectively trims #, $, and ? from the name.")

  (function target-symbol
    "Returns a symbol from the *TARGET-PACKAGE* whose name is formed by
FORMAT-STRING and FORMAT-ARGS.

See QTOOLS:*TARGET-PACKAGE*
See CL:INTERN
See CL:FORMAT")

  (function with-output-to-target-symbol
    "Same as WITH-OUTPUT-TO-STRING, but the result is translated to a *TARGET-PACKAGE* symbol.

See QTOOLS:TARGET-SYMBOL
See CL:WITH-OUTPUT-TO-STRING")

  (function write-qclass-name
    "Writes an appropriate translation of the QCLASS' name to STREAM.

Translation is as follows:
For every character, its uppercase equivalent is printed to stream,
with the exception of #\: which is printed as a #\-. Any number of
succeeding #\: s is translated to a single #\-.

KLUDGE: This approach does not currently take the readtable case 
into account. This will be problematic on systems where it matters.

See CL:CHAR-UPCASE")

  (function write-qmethod-name
    "Writes an appropriate translation of the QMETHOD's name to STREAM.

Translation is as follows:
If an uppercase alphabetic character is encountered and the previous
character was not already uppercase, #\- is printed. If #\_ is
encountered, it is treated as an uppercase character and printed
as #\_. A #\- would be more \"lispy\" to use for a #\_, but doing so
leads to method name clashes (most notably \"set_widget\" and 
\"setWidget\"). Any other character is printed as their uppercase
equivalent.

KLUDGE: This approach does not currently take the readtable case 
into account. This will be problematic on systems where it matters.

See CL:CHAR-UPCASE")
  
  (function method-needed-p
    "Returns T if the METHOD is considered to be useful for wrapper compilation.")

  (function method-symbol
    "Returns an appropriate symbol to use for the name of the wrapper function for METHOD.

See QT:QMETHOD-ENUM-P
See QTOOLS:CL-CONSTANT-NAME
See QT:QMETHOD-CTOR-P
See QT:QMETHOD-COPYCTOR-P
See QTOOLS:CL-CONSTRUCTOR-NAME
See QTOOLS:QMETHOD-OPERATOR-P
See QTOOLS:CL-OPERATOR-NAME
See QT:QMETHOD-STATIC-P
See QTOOLS:CL-STATIC-METHOD-NAME
See QTOOLS:CL-METHOD-NAME")

  (function process-method
    "Push the given METHOD onto its appropriate place in the method table, if it is needed.

See QTOOLS:METHOD-NEEDED-P
See QTOOLS:METHOD-SYMBOL
See QTOOLS:*QMETHODS*")

  (function process-all-methods
    "Clears the method table and generates all possible data for the currently available methods.
This also sets the *GENERATED-MODULES* to the proper value.

See QT:MAP-METHODS
See QTOOLS:PROCESS-METHOD
See QTOOLS:CLEAR-METHOD-INFO
See QTOOLS:*GENERATED-MODULES*
See QTOOLS:LOADED-SMOKE-MODULES")

  (function ensure-methods-processed
    "Ensures that all methods have been generated for the currently loaded smoke modules.

See QTOOLS:LOADED-SMOKE-MODULES
See QTOOLS:*GENERATED-MODULES*
See QTOOLS:PROCESS-ALL-METHODS")

  (function ensure-methods
    "Attempts to return the list of methods associated with METHOD.

METHOD can be a SYMBOL, LIST, FIXNUM, or STRING.
If no methods can be found in the method table, an error is signalled.")

  (function compile-wrapper
    "Compiles the wrapper function for METHOD.

This does not actually call CL:COMPILE, or change
the global environment in any way. It instead returns
a form that you can then truly compile, print, or write
to file, or whatever your intention is.

See QTOOLS:ENSURE-METHODS
See QT:QMETHOD-ENUM-P
See QTOOLS:COMPILE-CONSTANT
See QT:QMETHOD-CTOR-P
See QT:QMETHOD-COPYCTOR-P
See QTOOLS:COMPILE-CONSTRUCTOR
See QTOOLS:QMETHOD-OPERATOR-P
See QTOOLS:COMPILE-OPERATOR
See QT:QMETHOD-STATIC-P
See QTOOLS:COMPILE-STATIC-METHOD
See QTOOLS:COMPILE-METHOD")

  (function map-compile-all
    "Calls FUNCTION with the result of COMPILE-WRAPPER on all available methods.

See QTOOLS:COMPILE-WRAPPER
See QTOOLS:*QMETHODS*")

  (function generate-method-docstring
    "Generates a docstring that references all the Qt methods that a wrapper function can be used for.")

  (function generate-constant-docstring
    "Generates a docstring for an enum constant.")

  (function with-args
    "Calculates the proper argument list parts required for the given METHODS.")

  (function define-extern-inline-fun
    "Shorthand for EXPORT, DECLAIM INLINE, and DEFUN.")

  (function define-extern-macro
    "Shorthand for EXPORT and DEFMACRO."))

;; name-translation.lisp
(docs:define-docs
  (function to-method-name
    "Turns THING into a Qt method name.
If THING is a STRING, it is returned directly.
If THING is a SYMBOL, it is transformed by turning each
character after a hyphen into its uppercase equivalent
and dropping the hyphen. Therefore: foo-bar fooBar")

  (function qt-type-of
    "Attempts to determine a proper Qt type descriptor for the type of the OBJECT.

Look at the source to see the mappings.")

  (function qt-type-for
    "Attempts to determine the proper Qt type descriptor for the passed cl type name.

Look at the source to see the mappings.")

  (function to-type-name
    "Returns the type name for THING.

If THING is a string, it is returned directly.
If it is a symbol, either QT-TYPE-FOR for THING is
returned, or the STRING-DOWNCASE of THING.")

  (function cl-type-for
    "Attempts to determine the CL type for the given Qt type descriptor.

Look at the source to see the mappings.")

  (function eqt-type-of
    "Same as QT-TYPE-OF, but signals an error if no matching type could be found.")

  (function ecl-type-for
    "Same as CL-TYPE-FOR, but signals an error if no matching type could be found.")

  (function determined-type-method-name
    "Returns a method designator for the FUNCTION and ARGS.

The FUNCTION is transformed as by TO-METHOD-NAME.
Argument types are determined as follows:
If the argument is a CONS, the CAR is taken as a value (and thus discarded)
and the CDR is the literal type to take. Otherwise the type is determined
by EQT-TYPE-OF.")

  (function specified-type-method-name
    "Returns a method designator for the FUNCTION and ARGS.

The FUNCTION is transformed as by TO-METHOD-NAME. Each argument type is
determined as by TO-TYPE-NAME."))

;; precompile.lisp
(docs:define-docs
  (function write-forms
    "Writes all compileable forms to STREAM.

See QTOOLS:MAP-COMPILE-ALL")

  (function write-everything-to-file
    "Writes all compileable Qt method wrappers to PATHNAME.

PACKAGE designates in which package the symbols will live.
This makes it possible to deviate from the standard of
*TARGET-PACKAGE*. The value of QTOOLS:*TARGET-PACKAGE*
will be automatically set to this once the resulting file
is LOADed or compiled again.

See QTOOLS:WRITE-FORMS
See QTOOLS:*TARGET-PACKAGE*")

  (function q+-compile-and-load
    "Writes, compiles, and loads the file for all generated Qt wrapper functions.
If MODULES is passed, CommonQt is reloaded and only the given modules are loaded.

See WRITE-EVERYTHING-TO-FILE")
  
  (type smoke-module-system
    "A wrapper ASDF system class that only exists to ensure that a
given smoke module is loaded at compile and load time.")
  
  (function compile-smoke-module-system-definition
    "Creates an ASDF:DEFSYSTEM form for the MODULE.

See QTOOLS:SMOKE-MODULE-SYSTEM")

  (function write-smoke-module-system-file
    "Writes a SMOKE-MODULE-SYSTEM form to the given PATH.

See QTOOLS:COMPILE-SMOKE-MODULE-SYSTEM-DEFINITION"))

;; printing.lisp
(docs:define-docs
  (function define-print-method
    "Defines a method to print an object of CLASS.
CLASS can be either a common-lisp class type or a Qt class name.

Qt class names will take precedence, meaning that if CLASS resolves
to a name using FIND-QT-CLASS-NAME a QCLASS-PRINT method
is defined on the respective qt-class. Otherwise a PRINT-OBJECT method
is defined with the CLASS directly as specializer for the instance.

In cases where you need to define a method on a same-named CL class,
directly use DEFMETHOD on PRINT-OBJECT.

See PRINT-OBJECT")

  (function describe-print-method
    "Prints information about the print method for the specified class if possible."))

;; signal.lisp
(docs:define-docs
  (function generic-signal
    "Attempts to signal the function FUNCTION on OBJECT by determining the
types according to the run-time types of the values.

This is SLOW as the signal method has to be determined at run-time and it
is DANGEROUS as the type mapping are ambiguous or even unknown for certain
arguments and as such the wrong signal may be called or even one that does
not actually exist. If you want to explicitly specify the type of the
argument, wrap it in a CONS where the CAR is the value and the CDR is a
string for the according Qt type.

A compiler macro will try to statically determine types as best as possible,
so GENERIC-SIGNAL is save to use for static values.")

  (compiler-macro-function generic-signal
    "Attempts to predetermine as much type information for GENERIC-SIGNAL as possible.
If all types can be determined statically, EMIT-SIGNAL is used directly instead.")

  (function signal!
    "Macro for a more lisp-y writing of EMIT-SIGNAL.
Function should be a list of the METHOD-NAME followed by Qt argument types.
The effective method name is computed as per SPECIFIED-TYPE-METHOD-NAME.

OBJECT can be either a single object to signal to, or a list of objects.")

  (function connect!
    "Macro for a more lisp-y writing of CONNECT.
ORIGIN-FUNCTION and TARGET-FUNCTION should both be a list of the METHOD-NAME
followed by Qt argument types. The effective method name is computed as per
SPECIFIED-TYPE-METHOD-NAME.

ORIGIN and TARGET can both be either a single object or a list of objects
to connect with each other.")

  (function define-signal-method
    "Shorthand to define wrapper methods for the given signal.

NAME ::= signal | (signal method-name)
ARGS ::= ARG*
ARG  ::= qt-type | (qt-type*)

A methods with name NAME are generated that takes arguments the
object to signal and the specified arguments with their according types.
You may either specify a single type on each argument, or lists of
correlating types for each argument. Each type is resolved as per
ECL-TYPE-FOR to a type to use in the method specializers. The signal
method to call is computed as per SPECIFIED-TYPE-METHOD-NAME."))

;; toolkit.lisp
(docs:define-docs
  (function value
    "Accesses the VALUE of the object. This usually translates to (#_value object) unless overridden.")

  (function parent
    "accesses the PARENT of the object. This usually translates to (#_parent object) unless overridden.")

  (function qobject-alive-p
    "Returns T if the object is not null and not deleted.")

  (function maybe-delete-qobject
    "Deletes the object if possible.")

  (function qtenumcase
    "Similar to CASE:

KEYFORM  --- A form that evaluates to the key to compare against.
CASES    ::= CASE*
CASE     ::= (KEY form*)
KEY      ::= (OR form*) | FORM | t | otherwise")

  (function qtypecase
    "Analogous to CL:TYPECASE, but for Qt classes.

See QINSTANCEP")

  (function map-layout
    "Map all widgets and layouts on LAYOUT onto FUNCTION.")

  (function do-layout
    "Iterate over all WIDGETs on LAYOUT.")

  (function sweep-layout
    "Removes all widgets from the layout and finalizes them.")

  (function enumerate-method-descriptors
    "Returns a list of all possible method descriptors with NAME and ARGS.
Args may be either a list of direct types to use or a list of alternative types.
In the case of lists, the argument alternatives are taken in parallel.

Examples: 
 (.. foo '(a b)) => (\"foo(a,b)\")
 (.. foo '((a b))) => (\"foo(a)\" \"foo(b)\")
 (.. foo '((a b) (0 1))) => (\"foo(a,0)\" \"foo(b,1)\")")

  (function find-children
    "Find all children that are an instance of CHILD-CLASS

If FIRST-ONLY is non-NIL, only the first match is found, otherwise
a list is returned.

See QINSTANCEP")

  (function find-child
    "Find the first child that is an instance of CHILD-CLASS

See FIND-CHILDREN")

  (function ensure-class
    "Ensures to return a CLASS.
SYMBOL -> FIND-CLASS
CLASS  -> IDENTITY
STANDARD-OBJECT -> CLASS-OF")

  (function with-slots-bound
    "Turns into a WITH-SLOTS with all direct-slots of CLASS.
Class is resolved as per ENSURE-CLASS.")

  (function with-all-slots-bound
    "Turns into a WITH-SLOTS with all slots of CLASS.
Class is resolved as per ENSURE-CLASS.")

  (function split
    "Segregates items in LIST into separate lists if they mach an item in ITEMS.
The first item in the returned list is the list of unmatched items.

Example:
 (split '((0 a) (0 b) (1 a) (1 b) (2 c)) '(0 2) :key #'car)
 => '(((1 a) (1 b)) ((0 a) (0 b)) ((2 c))) ")

  (function with-compile-and-run
    "Compiles BODY in a lambda and funcalls it.")

  (function maybe-unwrap-quote
    "If it is a quote form, unwraps the contents. Otherwise returns it directly.")

  (function default-application-name
    "Attempts to find and return a default name to use for the application.")

  (function ensure-qapplication
    "Ensures that the QT:*QAPPLICATION* is available, potentially using NAME and ARGS to initialize it.

See QT:*QAPPLICATION*
See QT:ENSURE-SMOKE")

  (function ensure-qobject
    "Makes sure that THING is a usable qobject.

If THING is a symbol, it attempts to use MAKE-INSTANCE with it.")
  (function with-main-window
    "This is the main macro to start your application with.

It does the following:
1. Call ENSURE-QAPPLICATION with the provided NAME and QAPPLICATION-ARGS
2. Run the following in the main thread through TMT:WITH-BODY-IN-MAIN-THREAD
   if MAIN-THREAD is non-NIL and make it non-blocking if BLOCKING is NIL.
3. Establish a handler for ERROR that calls the ON-ERROR function if hit.
4. Bind WINDOW to the result of INSTANTIATOR, passed through ENSURE-QOBJECT
   (This means you can also just use the main window class' name)
5. Evaluate BODY
6. Call Q+:SHOW on WINDOW if SHOW is non-NIL
7. Call Q+:EXEC on *QAPPLICATION*
   This will enter the Qt application's main loop that won't exit until your
   application terminates.
8. Upon termination, call FINALIZE on WINDOW."))

;; widget-convenience.lisp
(docs:define-docs
  (function define-slot
    "Define a new SLOT on WIDGET-CLASS with ARGS.

ARGS is a list of arguments, where each item is a list of two values,
the first being the symbol used to bind the value within the function
body, and the second being a type specifier usable for the slot definition
and, if possible, as a specializer in the method. You may specify an
explicit type to use for the method specializer as a third item. If no
explicit type is passed, the Qt type is translated using CL-TYPE-FOR.

In effect this translates to a method definition with METHOD-NAME that
specialises (and binds) on WIDGET-CLASS, with additional required arguments
ARGS, and a SLOT declaration. Additionally, the body is wrapped in a
WITH-SLOTS-BOUND to allow for convenient slot access.

See QTOOLS:CL-TYPE-FOR
See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See CommonQt/slots")

  (function define-override
    "Define a new OVERRIDE on WIDGET-CLASS with ARGS.

This is translated to a method definition with METHOD-NAME that specialises
 (and binds) on WIDGET-CLASS, with ARGS appended to the list, and an OVERRIDE
declaration in the body. Additionally, the body is wrapped in a WITH-SLOTS-BOUND
to allow for convenient slot access.

See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See CommonQt/override")

  (function define-initializer
    "Defines a new initializer of METHOD-NAME on WIDGET-CLASS.

Initializers are functions that are run immediately after the widget has been
allocated by QT:NEW, but before any INITIALIZE-INSTANCE:AFTER methods are
executed. They are executed in order of highest PRIORITY first. 

This is translated to a method definition specialised (and bound) on WIDGET-CLASS
with a INITIALIZER declaration. The BODY is wrapped in a WITH-SLOTS-BOUND form.

See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND")

  (function define-finalizer
    "Defines a new finalizer of METHOD-NAME on WIDGET-CLASS.

Finalizers are functions that are run immediately after the widget has been
FINALIZED, but before the main FINALIZE method kicks in. This means that the
widget will still be allocated at the time. Finalizers are executed in order 
of highest PRIORITY first. 

This is translated to a method definition specialised (and bound) on WIDGET-CLASS
with a FINALIZER declaration. The BODY is wrapped in a WITH-SLOTS-BOUND form.

See CL+QT:DEFMETHOD
See QTOOLS:WITH-SLOTS-BOUND
See QTOOLS:FINALIZE")

  (function define-signal
    "Define a new SIGNAL on WIDGET-CLASS with ARGS.

This evaluates to a simple SET-WIDGET-CLASS-OPTION that adds a new :SIGNAL
definition to the WIDGET-CLASS. The signal signature is generated using
SPECIFIED-TYPE-METHOD-NAME.

See CommonQt/signals")

  (function define-subwidget
    "Defines a new sub-widget of NAME on WIDGET-CLASS.

What this means is that a finalized slot of NAME is added to WIDGET-CLASS
as well as an initializer function for the slot. The slot for the sub-widget 
is set to the value returned by the INITFORM, after which BODY is run. BODY
is wrapped in a WITH-SLOTS-BOUND form, so all slots are conveniently available.

See QTOOLS:DEFINE-INITIALIZER")

  (function remove-slot
    "Removes the SLOT definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS")

  (function remove-override
    "Removes the OVERRIDE definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the override.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS")

  (function remove-initializer
    "Removes the INITIALIZER definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS")

  (function remove-finalizer
    "Removes the FINALIZER definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS")

  (function remove-signal
    "Removes the SIGNAL definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the slot.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS")

  (function remove-subwidget
    "Removes the SUBWIDGET definition from the WIDGET-CLASS.

Note that this does not remove eventual methods associated with the subwidget.
It does however remove the class-slot and initializer of the subwidget.

See QTOOLS:REMOVE-WIDGET-CLASS-OPTION
See QTOOLS:ENSURE-CLASS"))

;; widget-defmethod.lisp
(docs:define-docs
  (variable *method*
    "Contains the whole DEFMETHOD form that is currently being processed.
If you modify the contents of this variable, the changes will be reflected
in the outputted method definition form. However, no declaration that is
processed by method-declarations will ever appear in the output.")

  (function method-declaration
    "Returns a function to process the method declaration NAME, if one exists.

See (SETF QTOOLS:METHOD-DECLARATION).")

  (function (setf method-declaration)
    "Sets the FUNCTION to be used to process method declarations of NAME.
The arguments of the function should parse the inner of the declaration.
E.g: (declare (foo bar baz)) could be captured by (a &optional b) with
A=>BAR, B=>BAZ. During evaluation of the function, the special variable
*METHOD* will be bound.

See QTOOLS:*METHOD*.")

  (function remove-method-declaration
    "Remove the method declaration processor function of NAME.")

  (function define-method-declaration
    "Define a new method declaration function of NAME.

See (SETF QTOOLS:METHOD-DECLARATION).")

  (function cl+qt:defmethod
    "Defines a new method.

This is identical to CL:DEFMETHOD with one exception:
The only difference is that declarations are scanned and
potentially specially processed. If a declaration is
recognised through METHOD-DECLARATION, it is taken out of
the method definition. The declaration processor function
then may or may not cause side-effects or spit out
additional forms to be output alongside the CL:DEFMETHOD
form.

See CL:DEFMETHOD.
See QTOOLS:METHOD-DECLARATION.")

  (function with-widget-class
    "Binds VARIABLE to the current symbol name of the widget class as used as a specializer in the method arguments list.
This also signals errors if there is no such specializer or if it is invalid."))

;; widget-menu.lisp
(docs:define-docs
  (variable *widget*
    "Bound to the class-name of the widget during component expansion and
bound to the class instance during initialization.")

  (function widget-actions
    "Returns a list of QAction instances that are active on the given CLASS.")

  (function (setf widget-actions)
    "Sets the list of QAction instances that are active on the given CLASS.")

  (function menu-content-type
    "Returns the function to process a menu content type NAME, if any.")

  (function (setf menu-content-type)
    "Sets the FUNCTION to process menu contents of type NAME.

The function should accept in the very least one argument, which
is the symbol of the current parent. The other arguments can be 
used to decompose the remainder of the content form. Expected are
two return values, the first being a form to call during
initialization and the second being a form to be run alongside
the initializer definition.")

  (function remove-menu-content-type
    "Removes the menu content type NAME.")

  (function define-menu-content-type
    "Defines a new menu content type processor NAME.

See (SETF QTOOLS:MENU-CONTENT-TYPE).")

  (function build-menu-content
    "Calls the appropriate function to parse menu content of TYPE.

See (SETF QTOOLS:MENU-CONTENT-TYPE).")

  (function make-chord
    "Transforms CHORD into a keychord string, if possible.")

  (function define-menu
    "Defines a menu on WIDGET-CLASS with NAME and CONTENTS.

By default the following content types are available:
  A :MENU form is followed by a menu text string and a
  body of content forms.

  A :SEPARATOR simply adds a separator at its point to
  the parent and takes no further arguments.

  An :ITEM form is followed by an identifier, which may
  be a symbol, string, or list. In the case of a symbol,
  the item is taken from the according slot on the widget.
  In the case of a string the string serves as the text
  for the item. For a list, the first serves as the text
  and the second as an input acceptable to MAKE-CHORD.
  The body of the item form can be arbitrary lisp forms
  to be executed when the item is triggered.

See QTOOLS:MAKE-CHORD."))

;; widget.lisp
(docs:define-docs
  (type widget-class
    "Metaclass for widgets storing necessary information.

The metadata stored in this is mostly responsible for two things:
 1) Providing access to a sequence of mutually independent
    initializers and finalizers for convenient setup and cleanup.
 2) Allowing after-the-fact out-of-form changes to the class
    definition, which is necessary to have for a distributed
    definition form syntax as provided by WIDGET-CONVENIENCE macros.
In order to modify the metadata, please look at SET/REMOVE-WIDGET-CLASS-OPTION.")

  (function widget-class-direct-options
    "Contains all the options passed to RE/INITIALIZE-INSTANCE when
the class is re/initialized directly through a DEFCLASS form.")
  
  (function widget-class-extern-options
    "Contains all the options that are added to the class definition
through external forms and thus need to be included and kept separate
from options directly specified in the class definition.")
  
  (function widget-class-initializers
    "A sorted list of functions to be called upon initialization.
This list is overwritten completely whenever the class is re/initialized.

See QTOOLS:CALL-INITIALIZERS")
  
  (function widget-class-finalizers
    "A sorted list of functions to be called upon finalization.
This list is overwritten completely whenever the class is re/initialized.

See QTOOLS:CALL-FINALIZERS")

  (type widget
    "Common superclass for all widgets in order to allow for
general initialization and cleanup forms that are standardised across all
widgets. 

See QTOOLS:DEFINE-WIDGET.")

  (function call-initializers
    "Calls all the initializers specified on CLASS in their proper sequence.

CLASS can be either an instance of a WIDGET-CLASS, a
WIDGET-CLASS itself, or a symbol naming the class.")

  (function call-finalizers
    "Calls all the finalizers specified on CLASS in their proper sequence.

CLASS can be either an instance of a WIDGET-CLASS, a
WIDGET-CLASS itself, or a symbol naming the class.")

  (function construct
    "This method is called during the initialization of a widget instance.
It MUST call QT:NEW on the widget at some point. Its primary
purpose is to give the user some way to manipulate which arguments
are passed to QT:NEW. By default, no arguments are passed.")

  (condition invalid-qt-superclass-hierarchy
    "Error that is signalled when attempting to define a class with a Qt-superclass
which is not possible to use with the given direct-superclasses due to a hierarchy clash.")

  (function check-qt-superclass-compatibility
    "Check whether the given QT-SUPERCLASS is permissible given the DIRECT-SUPERCLASSES.")

  (function setup-widget-class
    "This function should not be called directly, but is instead invoked by the appropriate functions
such as INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, and SOFTLY-REDEFINE-WIDGET-CLASS. In brief,
it concerns itself with proper option merging and filtering before passing it on to the CommonQt
and CLOS methods that process them.")

  (function softly-redefine-widget-class
    "Cause a soft redefinition of the given CLASS.

This will in effect cause a call to REINITIALIZE-INSTANCE with the proper
class options added from WIDGET-CLASS-DIRECT-OPTIONS, followed by a
FINALIZE-INHERITANCE call on the class.")

  (function widget-class-option-p
    "Tests if OPTION VALUE is already present on CLASS.
Returns the full option value if it can be found.

See QTOOLS:SET-WIDGET-CLASS-OPTION")

  (function set-widget-class-option
    "Sets a CLASS OPTION VALUE.

The value is identified and distinguished within the OPTION list
by TEST on KEY. If a matching list can be found, it is replaced
at the same position. Otherwise it is appended to the end of the
list. The order here is important to preserve load-order.

See QTOOLS:WIDGET-CLASS-EXTERN-OPTIONS.
See QTOOLS:SOFTLY-REDEFINE-WIDGET-CLASS.")

  (function remove-widget-class-option
    "Removes a CLASS OPTION value.

The value is identified and distinguished within the OPTION list
by TEST on KEY. If the first item in the sub-list is EQUAL to IDENTIFIER,
it is removed. This causes a call to SOFTLY-REDEFINE-WIDGET-CLASS.

See QTOOLS:WIDGET-CLASS-EXTERN-OPTIONS.
See QTOOLS:SOFTLY-REDEFINE-WIDGET-CLASS.")

  (function define-widget
    "Shorthand over DEFCLASS.

Adds WIDGET as direct-superclass if it does not appear as a
superclass to the specified direct-superclasses. Sets 
WIDGET-CLASS as metaclass and qt-class as the qt-superclass 
after resolving it through FIND-QT-CLASS-NAME.

All options are fused as per FUSE-ALISTS. You may therefore use
the same form multiple times."))
