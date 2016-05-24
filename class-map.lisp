#|
 This file is a part of Qtools
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.qtools)

(defvar *qt-class-vector*
  #("QAbstractAnimation"
    "QAbstractButton"
    "QAbstractEventDispatcher"
    "QAbstractExtensionFactory"
    "QAbstractExtensionManager"
    "QAbstractFileEngine"
    "QAbstractFileEngineHandler"
    "QAbstractFileEngineIterator"
    "QAbstractFontEngine"
    "QAbstractFormBuilder"
    "QAbstractGraphicsShapeItem"
    "QAbstractItemDelegate"
    "QAbstractItemModel"
    "QAbstractItemView"
    "QAbstractListModel"
    "QAbstractMessageHandler"
    "QAbstractNetworkCache"
    "QAbstractPrintDialog"
    "QAbstractProxyModel"
    "QAbstractScrollArea"
    "QAbstractSlider"
    "QAbstractSocket"
    "QAbstractSpinBox"
    "QAbstractState"
    "QAbstractTableModel"
    "QAbstractTextDocumentLayout"
    "QAbstractTransition"
    "QAbstractUriResolver"
    "QAbstractVideoBuffer"
    "QAbstractVideoSurface"
    "QAbstractXmlNodeModel"
    "QAbstractXmlReceiver"
    "QAccessible"
    "QAccessibleBridge"
    "QAccessibleBridgePlugin"
    "QAccessibleEvent"
    "QAccessibleInterface"
    "QAccessibleObject"
    "QAccessiblePlugin"
    "QAccessibleWidget"
    "QAction"
    "QActionEvent"
    "QActionGroup"
    "QAnimationGroup"
    "QApplication"
    "QAtomicInt"
    "QAtomicPointer"
    "QAudioDeviceInfo"
    "QAudioFormat"
    "QAudioInput"
    "QAudioOutput"
    "QAuthenticator"
    "QAxAggregated"
    "QAxBase"
    "QAxBindable"
    "QAxFactory"
    "QAxObject"
    "QAxScript"
    "QAxScriptEngine"
    "QAxScriptManager"
    "QAxWidget"
    "QBasicTimer"
    "QBitArray"
    "QBitmap"
    "QBoxLayout"
    "QBrush"
    "QBuffer"
    "QButtonGroup"
    "QByteArray"
    "QByteArrayMatcher"
    "QCache"
    "QCalendarWidget"
    "QCDEStyle"
    "QChar"
    "QCheckBox"
    "QChildEvent"
    "QCleanlooksStyle"
    "QClipboard"
    "QCloseEvent"
    "QColor"
    "QColorDialog"
    "QColormap"
    "QColumnView"
    "QComboBox"
    "QCommandLinkButton"
    "QCommonStyle"
    "QCompleter"
    "QConicalGradient"
    "QContextMenuEvent"
    "QContiguousCache"
    "QCopChannel"
    "QCoreApplication"
    "QCryptographicHash"
    "QCursor"
    "QCustomRasterPaintDevice"
    "QDataStream"
    "QDataWidgetMapper"
    "QDate"
    "QDateEdit"
    "QDateTime"
    "QDateTimeEdit"
    "QDBusAbstractAdaptor"
    "QDBusAbstractInterface"
    "QDBusArgument"
    "QDBusConnection"
    "QDBusConnectionInterface"
    "QDBusContext"
    "QDBusError"
    "QDBusInterface"
    "QDBusMessage"
    "QDBusObjectPath"
    "QDBusPendingCall"
    "QDBusPendingCallWatcher"
    "QDBusPendingReply"
    "QDBusReply"
    "QDBusServiceWatcher"
    "QDBusSignature"
    "QDBusUnixFileDescriptor"
    "QDBusVariant"
    "QDebug"
    "QDeclarativeComponent"
    "QDeclarativeContext"
    "QDeclarativeEngine"
    "QDeclarativeError"
    "QDeclarativeExpression"
    "QDeclarativeExtensionPlugin"
    "QDeclarativeImageProvider"
    "QDeclarativeItem"
    "QDeclarativeListProperty"
    "QDeclarativeListReference"
    "QDeclarativeNetworkAccessManagerFactory"
    "QDeclarativeParserStatus"
    "QDeclarativeProperty"
    "QDeclarativePropertyMap"
    "QDeclarativePropertyValueSource"
    "QDeclarativeScriptString"
    "QDeclarativeView"
    "QDecoration"
    "QDecorationDefault"
    "QDecorationFactory"
    "QDecorationPlugin"
    "QDesignerActionEditorInterface"
    "QDesignerContainerExtension"
    "QDesignerCustomWidgetCollectionInterface"
    "QDesignerCustomWidgetInterface"
    "QDesignerDynamicPropertySheetExtension"
    "QDesignerFormEditorInterface"
    "QDesignerFormWindowCursorInterface"
    "QDesignerFormWindowInterface"
    "QDesignerFormWindowManagerInterface"
    "QDesignerMemberSheetExtension"
    "QDesignerObjectInspectorInterface"
    "QDesignerPropertyEditorInterface"
    "QDesignerPropertySheetExtension"
    "QDesignerTaskMenuExtension"
    "QDesignerWidgetBoxInterface"
    "QDesktopServices"
    "QDesktopWidget"
    "QDial"
    "QDialog"
    "QDialogButtonBox"
    "QDir"
    "QDirectPainter"
    "QDirIterator"
    "QDockWidget"
    "QDomAttr"
    "QDomCDATASection"
    "QDomCharacterData"
    "QDomComment"
    "QDomDocument"
    "QDomDocumentFragment"
    "QDomDocumentType"
    "QDomElement"
    "QDomEntity"
    "QDomEntityReference"
    "QDomImplementation"
    "QDomNamedNodeMap"
    "QDomNode"
    "QDomNodeList"
    "QDomNotation"
    "QDomProcessingInstruction"
    "QDomText"
    "QDoubleSpinBox"
    "QDoubleValidator"
    "QDrag"
    "QDragEnterEvent"
    "QDragLeaveEvent"
    "QDragMoveEvent"
    "QDropEvent"
    "QDynamicPropertyChangeEvent"
    "QEasingCurve"
    "QElapsedTimer"
    "QErrorMessage"
    "QEvent"
    "QEventLoop"
    "QEventTransition"
    "QExplicitlySharedDataPointer"
    "QExtensionFactory"
    "QExtensionManager"
    "QFile"
    "QFileDialog"
    "QFileIconProvider"
    "QFileInfo"
    "QFileOpenEvent"
    "QFileSystemModel"
    "QFileSystemWatcher"
    "QFinalState"
    "QFlag"
    "QFlags"
    "QFocusEvent"
    "QFocusFrame"
    "QFont"
    "QFontComboBox"
    "QFontDatabase"
    "QFontDialog"
    "QFontEngineInfo"
    "QFontEnginePlugin"
    "QFontInfo"
    "QFontMetrics"
    "QFontMetricsF"
    "QFormBuilder"
    "QFormLayout"
    "QFrame"
    "QFSFileEngine"
    "QFtp"
    "QFuture"
    "QFutureIterator"
    "QFutureSynchronizer"
    "QFutureWatcher"
    "QGenericArgument"
    "QGenericMatrix"
    "QGenericPlugin"
    "QGenericPluginFactory"
    "QGenericReturnArgument"
    "QGesture"
    "QGestureEvent"
    "QGestureRecognizer"
    "QGLBuffer"
    "QGLColormap"
    "QGLContext"
    "QGLFormat"
    "QGLFramebufferObject"
    "QGLFramebufferObjectFormat"
    "QGLFunctions"
    "QGLPixelBuffer"
    "QGLShader"
    "QGLShaderProgram"
    "QGLWidget"
    "QGlyphRun"
    "QGradient"
    "QGraphicsAnchor"
    "QGraphicsAnchorLayout"
    "QGraphicsBlurEffect"
    "QGraphicsColorizeEffect"
    "QGraphicsDropShadowEffect"
    "QGraphicsEffect"
    "QGraphicsEllipseItem"
    "QGraphicsGridLayout"
    "QGraphicsItem"
    "QGraphicsItemAnimation"
    "QGraphicsItemGroup"
    "QGraphicsLayout"
    "QGraphicsLayoutItem"
    "QGraphicsLinearLayout"
    "QGraphicsLineItem"
    "QGraphicsObject"
    "QGraphicsOpacityEffect"
    "QGraphicsPathItem"
    "QGraphicsPixmapItem"
    "QGraphicsPolygonItem"
    "QGraphicsProxyWidget"
    "QGraphicsRectItem"
    "QGraphicsRotation"
    "QGraphicsScale"
    "QGraphicsScene"
    "QGraphicsSceneContextMenuEvent"
    "QGraphicsSceneDragDropEvent"
    "QGraphicsSceneEvent"
    "QGraphicsSceneHelpEvent"
    "QGraphicsSceneHoverEvent"
    "QGraphicsSceneMouseEvent"
    "QGraphicsSceneMoveEvent"
    "QGraphicsSceneResizeEvent"
    "QGraphicsSceneWheelEvent"
    "QGraphicsSimpleTextItem"
    "QGraphicsSvgItem"
    "QGraphicsTextItem"
    "QGraphicsTransform"
    "QGraphicsView"
    "QGraphicsWebView"
    "QGraphicsWidget"
    "QGridLayout"
    "QGroupBox"
    "QGtkStyle"
    "QHash"
    "QHashIterator"
    "QHBoxLayout"
    "QHeaderView"
    "QHelpContentItem"
    "QHelpContentModel"
    "QHelpContentWidget"
    "QHelpEngine"
    "QHelpEngineCore"
    "QHelpEvent"
    "QHelpIndexModel"
    "QHelpIndexWidget"
    "QHelpSearchEngine"
    "QHelpSearchQuery"
    "QHelpSearchQueryWidget"
    "QHelpSearchResultWidget"
    "QHideEvent"
    "QHistoryState"
    "QHostAddress"
    "QHostInfo"
    "QHoverEvent"
    "QHttpMultiPart"
    "QHttpPart"
    "QIcon"
    "QIconDragEvent"
    "QIconEngine"
    "QIconEnginePlugin"
    "QIconEnginePluginV2"
    "QIconEngineV2"
    "QIdentityProxyModel"
    "QImage"
    "QImageIOHandler"
    "QImageIOPlugin"
    "QImageReader"
    "QImageWriter"
    "QInputContext"
    "QInputContextFactory"
    "QInputContextPlugin"
    "QInputDialog"
    "QInputEvent"
    "QInputMethodEvent"
    "QIntValidator"
    "QIODevice"
    "QItemDelegate"
    "QItemEditorCreator"
    "QItemEditorCreatorBase"
    "QItemEditorFactory"
    "QItemSelection"
    "QItemSelectionModel"
    "QItemSelectionRange"
    "QKbdDriverFactory"
    "QKbdDriverPlugin"
    "QKeyEvent"
    "QKeyEventTransition"
    "QKeySequence"
    "QLabel"
    "QLatin1Char"
    "QLatin1String"
    "QLayout"
    "QLayoutItem"
    "QLCDNumber"
    "QLibrary"
    "QLibraryInfo"
    "QLine"
    "QLinearGradient"
    "QLineEdit"
    "QLineF"
    "QLinkedList"
    "QLinkedListIterator"
    "QList"
    "QListIterator"
    "QListView"
    "QListWidget"
    "QListWidgetItem"
    "QLocale"
    "QLocalServer"
    "QLocalSocket"
    "QMacCocoaViewContainer"
    "QMacNativeWidget"
    "QMacPasteboardMime"
    "QMacStyle"
    "QMainWindow"
    "QMap"
    "QMapIterator"
    "QMargins"
    "QMatrix4x4"
    "QMdiArea"
    "QMdiSubWindow"
    "QMenu"
    "QMenuBar"
    "QMessageBox"
    "QMetaClassInfo"
    "QMetaEnum"
    "QMetaMethod"
    "QMetaObject"
    "QMetaProperty"
    "QMetaType"
    "QMimeData"
    "QModelIndex"
    "QMotifStyle"
    "QMouseDriverFactory"
    "QMouseDriverPlugin"
    "QMouseEvent"
    "QMouseEventTransition"
    "QMoveEvent"
    "QMovie"
    "QMultiHash"
    "QMultiMap"
    "QMutableHashIterator"
    "QMutableLinkedListIterator"
    "QMutableListIterator"
    "QMutableMapIterator"
    "QMutableSetIterator"
    "QMutableVectorIterator"
    "QMutex"
    "QMutexLocker"
    "QNetworkAccessManager"
    "QNetworkAddressEntry"
    "QNetworkCacheMetaData"
    "QNetworkConfiguration"
    "QNetworkConfigurationManager"
    "QNetworkCookie"
    "QNetworkCookieJar"
    "QNetworkDiskCache"
    "QNetworkInterface"
    "QNetworkProxy"
    "QNetworkProxyFactory"
    "QNetworkProxyQuery"
    "QNetworkReply"
    "QNetworkRequest"
    "QNetworkSession"
    "QObject"
    "QObjectCleanupHandler"
    "QPageSetupDialog"
    "QPaintDevice"
    "QPaintEngine"
    "QPaintEngineState"
    "QPainter"
    "QPainterPath"
    "QPainterPathStroker"
    "QPaintEvent"
    "QPair"
    "QPalette"
    "QPanGesture"
    "QParallelAnimationGroup"
    "QPauseAnimation"
    "QPen"
    "QPersistentModelIndex"
    "QPicture"
    "QPinchGesture"
    "QPixmap"
    "QPixmapCache"
    "QPlainTextDocumentLayout"
    "QPlainTextEdit"
    "QPlastiqueStyle"
    "QPlatformCursor"
    "QPlatformCursorImage"
    "QPlatformFontDatabase"
    "QPlatformWindowFormat"
    "QPluginLoader"
    "QPoint"
    "QPointer"
    "QPointF"
    "QPolygon"
    "QPolygonF"
    "QPrintDialog"
    "QPrintEngine"
    "QPrinter"
    "QPrinterInfo"
    "QPrintPreviewDialog"
    "QPrintPreviewWidget"
    "QProcess"
    "QProcessEnvironment"
    "QProgressBar"
    "QProgressDialog"
    "QPropertyAnimation"
    "QProxyScreen"
    "QProxyScreenCursor"
    "QProxyStyle"
    "QPushButton"
    "QTouchEventSequence"
    "QQuaternion"
    "QQueue"
    "QRadialGradient"
    "QRadioButton"
    "QRasterPaintEngine"
    "QRawFont"
    "QReadLocker"
    "QReadWriteLock"
    "QRect"
    "QRectF"
    "QRegExp"
    "QRegExpValidator"
    "QRegion"
    "QResizeEvent"
    "QResource"
    "QRubberBand"
    "QRunnable"
    "QS60MainApplication"
    "QS60MainAppUi"
    "QS60MainDocument"
    "QS60Style"
    "QScopedArrayPointer"
    "QScopedPointer"
    "QScopedValueRollback"
    "QScreen"
    "QScreenCursor"
    "QScreenDriverFactory"
    "QScreenDriverPlugin"
    "QScriptable"
    "QScriptClass"
    "QScriptClassPropertyIterator"
    "QScriptContext"
    "QScriptContextInfo"
    "QScriptEngine"
    "QScriptEngineAgent"
    "QScriptEngineDebugger"
    "QScriptExtensionPlugin"
    "QScriptProgram"
    "QScriptString"
    "QScriptSyntaxCheckResult"
    "QScriptValue"
    "QScriptValueIterator"
    "QScrollArea"
    "QScrollBar"
    "QSemaphore"
    "QSequentialAnimationGroup"
    "QSessionManager"
    "QSet"
    "QSetIterator"
    "QSettings"
    "QSharedData"
    "QSharedDataPointer"
    "QSharedMemory"
    "QSharedPointer"
    "QShortcut"
    "QShortcutEvent"
    "QShowEvent"
    "QSignalMapper"
    "QSignalSpy"
    "QSignalTransition"
    "QSimpleXmlNodeModel"
    "QSize"
    "QSizeF"
    "QSizeGrip"
    "QSizePolicy"
    "QSlider"
    "QSocketNotifier"
    "QSortFilterProxyModel"
    "QSound"
    "QSourceLocation"
    "QSpacerItem"
    "QSpinBox"
    "QSplashScreen"
    "QSplitter"
    "QSplitterHandle"
    "QSqlDatabase"
    "QSqlDriver"
    "QSqlDriverCreator"
    "QSqlDriverCreatorBase"
    "QSqlDriverPlugin"
    "QSqlError"
    "QSqlField"
    "QSqlIndex"
    "QSqlQuery"
    "QSqlQueryModel"
    "QSqlRecord"
    "QSqlRelation"
    "QSqlRelationalDelegate"
    "QSqlRelationalTableModel"
    "QSqlResult"
    "QSqlTableModel"
    "QSslCertificate"
    "QSslCipher"
    "QSslConfiguration"
    "QSslError"
    "QSslKey"
    "QSslSocket"
    "QStack"
    "QStackedLayout"
    "QStackedWidget"
    "QStandardItem"
    "QStandardItemEditorCreator"
    "QStandardItemModel"
    "QState"
    "QStateMachine"
    "QStaticText"
    "QStatusBar"
    "QStatusTipEvent"
    "QString"
    "QStringList"
    "QStringListModel"
    "QStringMatcher"
    "QStringRef"
    "QStyle"
    "QStyledItemDelegate"
    "QStyleFactory"
    "QStyleHintReturn"
    "QStyleHintReturnMask"
    "QStyleHintReturnVariant"
    "QStyleOption"
    "QStyleOptionButton"
    "QStyleOptionComboBox"
    "QStyleOptionComplex"
    "QStyleOptionDockWidget"
    "QStyleOptionFocusRect"
    "QStyleOptionFrame"
    "QStyleOptionFrameV2"
    "QStyleOptionFrameV3"
    "QStyleOptionGraphicsItem"
    "QStyleOptionGroupBox"
    "QStyleOptionHeader"
    "QStyleOptionMenuItem"
    "QStyleOptionProgressBar"
    "QStyleOptionProgressBarV2"
    "QStyleOptionQ3DockWindow"
    "QStyleOptionQ3ListView"
    "QStyleOptionQ3ListViewItem"
    "QStyleOptionRubberBand"
    "QStyleOptionSizeGrip"
    "QStyleOptionSlider"
    "QStyleOptionSpinBox"
    "QStyleOptionTab"
    "QStyleOptionTabBarBase"
    "QStyleOptionTabBarBaseV2"
    "QStyleOptionTabV2"
    "QStyleOptionTabV3"
    "QStyleOptionTabWidgetFrame"
    "QStyleOptionTabWidgetFrameV2"
    "QStyleOptionTitleBar"
    "QStyleOptionToolBar"
    "QStyleOptionToolBox"
    "QStyleOptionToolBoxV2"
    "QStyleOptionToolButton"
    "QStyleOptionViewItem"
    "QStyleOptionViewItemV2"
    "QStyleOptionViewItemV3"
    "QStyleOptionViewItemV4"
    "QStylePainter"
    "QStylePlugin"
    "QSupportedWritingSystems"
    "QSvgGenerator"
    "QSvgRenderer"
    "QSvgWidget"
    "QSwipeGesture"
    "QSymbianEvent"
    "QSymbianGraphicsSystemHelper"
    "QSyntaxHighlighter"
    "QSysInfo"
    "QSystemLocale"
    "QSystemSemaphore"
    "QSystemTrayIcon"
    "QTabBar"
    "QTabletEvent"
    "QTableView"
    "QTableWidget"
    "QTableWidgetItem"
    "QTableWidgetSelectionRange"
    "QTabWidget"
    "QTapAndHoldGesture"
    "QTapGesture"
    "QTcpServer"
    "QTcpSocket"
    "QTemporaryFile"
    "QTestEventList"
    "QTextBlock"
    "QTextBlockFormat"
    "QTextBlockGroup"
    "QTextBlockUserData"
    "QTextBoundaryFinder"
    "QTextBrowser"
    "QTextCharFormat"
    "QTextCodec"
    "QTextCodecPlugin"
    "QTextCursor"
    "QTextDecoder"
    "QTextDocument"
    "QTextDocumentFragment"
    "QTextDocumentWriter"
    "QTextEdit"
    "QTextEncoder"
    "QTextFormat"
    "QTextFragment"
    "QTextFrame"
    "QTextFrameFormat"
    "QTextImageFormat"
    "QTextInlineObject"
    "QTextItem"
    "QTextLayout"
    "QTextLength"
    "QTextLine"
    "QTextList"
    "QTextListFormat"
    "QTextObject"
    "QTextObjectInterface"
    "QTextOption"
    "QTextStream"
    "QTextTable"
    "QTextTableCell"
    "QTextTableCellFormat"
    "QTextTableFormat"
    "QThread"
    "QThreadPool"
    "QThreadStorage"
    "QTileRules"
    "QTime"
    "QTimeEdit"
    "QTimeLine"
    "QTimer"
    "QTimerEvent"
    "QToolBar"
    "QToolBox"
    "QToolButton"
    "QToolTip"
    "QTouchEvent"
    "QTransform"
    "QTranslator"
    "QTreeView"
    "QTreeWidget"
    "QTreeWidgetItem"
    "QTreeWidgetItemIterator"
    "QUdpSocket"
    "QUiLoader"
    "QUndoCommand"
    "QUndoGroup"
    "QUndoStack"
    "QUndoView"
    "QUrl"
    "QUrlInfo"
    "QUuid"
    "QValidator"
    "QVariant"
    "QVariantAnimation"
    "QVarLengthArray"
    "QVBoxLayout"
    "QVector"
    "QVector2D"
    "QVector3D"
    "QVector4D"
    "QVectorIterator"
    "QVideoFrame"
    "QVideoSurfaceFormat"
    "QWaitCondition"
    "QWeakPointer"
    "QWebDatabase"
    "QWebElement"
    "QWebElementCollection"
    "QWebFrame"
    "QWebHistory"
    "QWebHistoryInterface"
    "QWebHistoryItem"
    "QWebHitTestResult"
    "QWebInspector"
    "QWebPage"
    "QWebPluginFactory"
    "QWebSecurityOrigin"
    "QWebSettings"
    "QWebView"
    "QWhatsThis"
    "QWhatsThisClickedEvent"
    "QWheelEvent"
    "QWidget"
    "QWidgetAction"
    "QWidgetItem"
    "QWindowsMime"
    "QWindowsStyle"
    "QWindowStateChangeEvent"
    "QWindowsVistaStyle"
    "QWindowsXPStyle"
    "QWizard"
    "QWizardPage"
    "QWriteLocker"
    "QWSCalibratedMouseHandler"
    "QWSClient"
    "QWSEmbedWidget"
    "QWSEvent"
    "QWSGLWindowSurface"
    "QWSInputMethod"
    "QWSKeyboardHandler"
    "QWSMouseHandler"
    "QWSPointerCalibrationData"
    "QWSScreenSaver"
    "QWSServer"
    "QWSWindow"
    "QX11EmbedContainer"
    "QX11EmbedWidget"
    "QX11Info"
    "QXmlAttributes"
    "QXmlContentHandler"
    "QXmlDeclHandler"
    "QXmlDefaultHandler"
    "QXmlDTDHandler"
    "QXmlEntityResolver"
    "QXmlErrorHandler"
    "QXmlFormatter"
    "QXmlInputSource"
    "QXmlItem"
    "QXmlLexicalHandler"
    "QXmlLocator"
    "QXmlName"
    "QXmlNamePool"
    "QXmlNamespaceSupport"
    "QXmlNodeModelIndex"
    "QXmlParseException"
    "QXmlQuery"
    "QXmlReader"
    "QXmlResultItems"
    "QXmlSchema"
    "QXmlSchemaValidator"
    "QXmlSerializer"
    "QXmlSimpleReader"
    "QXmlStreamAttribute"
    "QXmlStreamAttributes"
    "QXmlStreamEntityDeclaration"
    "QXmlStreamEntityResolver"
    "QXmlStreamNamespaceDeclaration"
    "QXmlStreamNotationDeclaration"
    "QXmlStreamReader"
    "QXmlStreamWriter"))

(defvar *qt-class-map* (let ((table (make-hash-table :test 'equalp :size (length *qt-class-vector*))))
                         (loop for class across *qt-class-vector*
                               do (setf (gethash class table) class))
                         table))

(defun find-qt-class-name (designator)
  (gethash (cl-ppcre:regex-replace-all "-" (string designator) "") *qt-class-map*))

(defun eqt-class-name (designator)
  (etypecase designator
    (string designator)
    (symbol (or (find-qt-class-name designator)
                (error "No corresponding Qt class found for ~s." designator)))))
