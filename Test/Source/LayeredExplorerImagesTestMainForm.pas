Unit LayeredExplorerImagesTestMainForm;

Interface

Uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs, System.ImageList, Vcl.ImgList, Vcl.ComCtrls;

Type
  TForm1 = Class(TForm)
    lvImages: TListView;
    ilImages: TImageList;
    procedure FormCreate(Sender: TObject);
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

  TScope = (
    scNone,
    scGlobal,
    scLocal,
    scPrivate,
    scProtected,
    scPublic,
    scPublished,
    scFriend
  );

  TBADIImageIndex = (
    iiNone,

    iiModule,

    iiDocConflictFolder,
    iiDocConflictIncorrect,
    iiDocConflictItem,
    iiDocConflictMissing,

    iiErrorFolder,
    iiError,
    iiWarningFolder,
    iiWarning,

    iiHintFolder,
    iiHint,

    iiUsesLabel,
    iiUsesItem,

    iiPublicTypesLabel,
    iiPublicType,

    iiRecordsLabel,
    iiPublicRecord,

    iiFieldsLabel,
    iiPublicField,

    iiObjectsLabel,
    iiPublicObject,

    iiPublicConstructor,
    iiPublicDestructor,
    iiPublicProcedure,
    iiPublicFunction,

    iiClassesLabel,
    iiPublicClass,

    iiPropertiesLabel,
    iiPublicProperty,

    iiInterfacesLabel,
    iiPublicInterface,

    iiDispInterfacesLabel,
    iiPublicDispInterface,

    iiPublicConstantsLabel,
    iiPublicConstant,

    iiPublicResourceStringsLabel,
    iiPublicResourceString,

    iiPublicVariablesLabel,
    iiPublicVariable,

    iiPublicThreadVarsLabel,
    iiPublicThreadVar,

    iiPublicClassVariablesLabel,
    iiPublicClassVariable,

    iiExportedHeadingsLabel,

    iiExportedFunctionsLabel,
    iiPublicExportedFunction,

    iiPublicLabelsLabel,
    iiPublicLabel,

    iiImplementedMethods,
    iiMethodsLabel,

    iiInitialization,
    iiFinalization,

    iiToDoFolder,
    iiToDoItem,

    iiUnknownClsObj
  );

  TImageIndexInfo = Record
    FResourceName : String;
    FMaskColour : TColor;
  End;

Const
  BADIScopeList : Array[Low(TScope)..High(TScope)] Of TImageIndexInfo = (
    (FResourceName : 'NoneMask';                      FMaskColour: clLime),
    (FResourceName : 'GlobalMask';                    FMaskColour: clLime),
    (FResourceName : 'LocalMask';                     FMaskColour: clLime),
    (FResourceName : 'PrivateMask';                   FMaskColour: clLime),
    (FResourceName : 'ProtectedMask';                 FMaskColour: clLime),
    (FResourceName : 'PublicMask';                    FMaskColour: clLime),
    (FResourceName : 'PublishedMask';                 FMaskColour: clLime),
    (FResourceName : 'FriendMask';                    FMaskColour: clLime)
  );

  BADIImageList : Array[Succ(Low(TBADIImageIndex))..High(TBADIImageIndex)] Of TImageIndexInfo = (
    (FResourceName : 'Module';                        FMaskColour: clLime),

    (FResourceName : 'DocConflictFolder';             FMaskColour: clLime),
    (FResourceName : 'DocConflictIncorrect';          FMaskColour: clLime),
    (FResourceName : 'DocConflictItem';               FMaskColour: clLime),
    (FResourceName : 'DocConflictMissing';            FMaskColour: clLime),

    (FResourceName : 'ErrorFolder';                   FMaskColour: clLime),
    (FResourceName : 'Error';                         FMaskColour: clLime),
    (FResourceName : 'WarningFolder';                 FMaskColour: clLime),
    (FResourceName : 'Warning';                       FMaskColour: clLime),
    (FResourceName : 'HintFolder';                    FMaskColour: clFuchsia),
    (FResourceName : 'Hint';                          FMaskColour: clFuchsia),

    (FResourceName : 'UsesLabel';                     FMaskColour: clLime),
    (FResourceName : 'UsesItem';                      FMaskColour: clLime),

    (FResourceName : 'PublicTypesLabel';              FMaskColour: clLime),
    (FResourceName : 'PublicType';                    FMaskColour: clLime),

    (FResourceName : 'RecordsLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicRecord';                  FMaskColour: clLime),

    (FResourceName : 'FieldsLabel';                   FMaskColour: clLime),
    (FResourceName : 'PublicField';                   FMaskColour: clLime),

    (FResourceName : 'ObjectsLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicObject';                  FMaskColour: clLime),

    (FResourceName : 'PublicConstructor';             FMaskColour: clLime),
    (FResourceName : 'PublicDestructor';              FMaskColour: clFuchsia),
    (FResourceName : 'PublicProcedure';               FMaskColour: clLime),
    (FResourceName : 'PublicFunction';                FMaskColour: clLime),

    (FResourceName : 'ClassesLabel';                  FMaskColour: clLime),
    (FResourceName : 'PublicClass';                   FMaskColour: clLime),

    (FResourceName : 'PropertyLabel';                 FMaskColour: clFuchsia),
    (FResourceName : 'PublicProperty';                FMaskColour: clFuchsia),

    (FResourceName : 'InterfacesLabel';               FMaskColour: clLime),
    (FResourceName : 'PublicInterface';               FMaskColour: clLime),

    (FResourceName : 'DispInterfaceSLabel';           FMaskColour: clLime),
    (FResourceName : 'PublicDispInterface';           FMaskColour: clLime),

    (FResourceName : 'PublicConstantsLabel';          FMaskColour: clLime),
    (FResourceName : 'PublicConst';                   FMaskColour: clLime),

    (FResourceName : 'PublicResourceStringsLabel';    FMaskColour: clLime),
    (FResourceName : 'PublicResourceString';          FMaskColour: clLime),

    (FResourceName : 'PublicVariablesLabel';          FMaskColour: clLime),
    (FResourceName : 'PublicVariable';                FMaskColour: clLime),

    (FResourceName : 'PublicThreadVarsLabel';         FMaskColour: clLime),
    (FResourceName : 'PublicThreadVar';               FMaskColour: clLime),

    (FResourceName : 'PublicClassVariablesLabel';     FMaskColour: clLime),
    (FResourceName : 'PublicClassVariable';           FMaskColour: clLime),

    (FResourceName : 'ExportedHeadingsLabel';         FMaskColour: clLime),

    (FResourceName : 'ExportedFunctionsLabel';        FMaskColour: clLime),
    (FResourceName : 'PublicExportedFunction';        FMaskColour: clLime),

    (FResourceName : 'PublicLabelsLabel';             FMaskColour: clLime),
    (FResourceName : 'PublicLabel';                   FMaskColour: clLime),

    (FResourceName : 'MethodsLabel';                  FMaskColour: clLime),
    (FResourceName : 'ImplementedMethodsLabel';       FMaskColour: clLime),

    (FResourceName : 'InitializationLabel';           FMaskColour: clFuchsia),
    (FResourceName : 'FinalizationLabel';             FMaskColour: clLime),

    (FResourceName : 'TodoFolder';                    FMaskColour: clLime),
    (FResourceName : 'TodoItem';                      FMaskColour: clLime),

    (FResourceName : 'UnknownClsObj';                 FMaskColour: clLime)
  );

Var
  Form1: TForm1;

Implementation

Uses
  CodeSiteLogging,
  TypInfo;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  iScope: TScope;
  iImage : TBADIImageIndex;
  Item : TListItem;
  MainImage, ScopeImage : TBitMap;
  x: Integer;
  y: Integer;
  R : TRect;

begin
  R := Rect(0, 0, 11, 11);
  CodeSite.TraceMethod(Self, 'FormCreate', tmoTiming);
  MainImage := TBitMap.Create;
  Try
    ScopeImage := TBitmap.Create;
    Try
      For iImage := Succ(Low(TBADIImageIndex)) To High(TBADIImageIndex) Do
        For iScope := Low(TScope) To High(TScope) Do
          Begin
              MainImage.LoadFromResourceName(hInstance, BADIImageList[iImage].FResourceName);
              //MainImage.TransparentColor := BADIScopeList[iScope].FMaskColour;
              ScopeImage.LoadFromResourceName(hInstance, BADIScopeList[iScope].FResourceName);
              //ScopeImage.TransparentColor := BADIScopeList[iScope].FMaskColour;
              //MainImage.Canvas.CopyMode := cmMergePaint;
              //MainImage.Canvas.BrushCopy(R, ScopeImage, R, BADIScopeList[iScope].FMaskColour);
              For x := 0 To 11 Do
                For y := 0 To 11 Do
                  If ScopeImage.Canvas.Pixels[x, y] <> BADIScopeList[iScope].FMaskColour Then
                    MainImage.Canvas.Pixels[x, y] := ScopeImage.Canvas.Pixels[x, y];
              ilImages.AddMasked(MainImage, BADIImageList[iImage].FMaskColour);
              //ilImages.GetInstRes(
              //  hInstance,
              //  rtBitmap,
              //  BADIImageList[iImage].FResourceName,
              //  16,
              //  [lrDefaultColor],
              //  BADIImageList[iImage].FMaskColour);
            Item := lvImages.Items.Add;
            Item.Caption :=
              Format('%d)', [lvimages.Items.Count]) +
              GetEnumName(TypeInfo(TScope), Ord(iScope)) + '::' +
              GetEnumName(TypeInfo(TBADIImageIndex), Ord(iImage));
            Item.ImageIndex := Pred(ilImages.Count)
          End;
    Finally
      ScopeImage.Free;
    End;
  Finally
    MainImage.Free;
  End;
end;

End.


