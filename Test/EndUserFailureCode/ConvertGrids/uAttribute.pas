unit uAttribute;

interface

uses
  SysUtils, Classes, Controls, Windows, Graphics, uSCMConst;

type
  // !!! Keep this in sync with MetaTypeNames !!!
  TMetaType = (mtID, mtCallNumber, mtInteger, mtIntCalc, mtDouble, mtCurrency,
               mtString, mtStringCombo, mtStringSelect, mtDateTime, mtDate,
               mtTime, mtTreeCombo, mtMemo, mtHyperlink, mtRadio, mtCheck,
               mtBOCombo, mtBOSearch, mtBOTree, mtBOList, mtState, mtUnknown
               );
  TMetaTypes = Set of TMetaType;

const
  //* Define the names for the TMetaType elements.
  MetaTypeNames : array [mtID..mtUnknown] of string =
    (
      'mtID', 'mtCallNumber', 'mtInteger', 'mtIntCalc', 'mtDouble', 'mtCurrency',
      'mtString', 'mtStringCombo', 'mtStringSelect', 'mtDateTime', 'mtDate',
      'mtTime', 'mtTreeCombo', 'mtMemo', 'mtHyperlink', 'mtRadio', 'mtCheck',
      'mtBOCombo', 'mtBOSearch', 'mtBOTree', 'mtBOList', 'mtState', 'mtUnknown'
    );

{   0: mtID
    1: mtCallNumber
    2: mtInteger
    3: mtIntCalc
    4: mtDouble
    5: mtCurrency
    6: mtString
    7: mtStringCombo
    8: mtStringSelect
    9: mtDateTime
   10: mtDate
   11: mtTime
   12: mtTreeCombo
   13: mtMemo
   14: mtHyperlink
   15: mtRadio
   16: mtCheck
   17: mtBOCombo
   18: mtBOSearch
   19: mtBOTree
   20: mtBOList
   21: mtState
   22: mtUnknown
}

type
  TBOFormInfo = class
    ClassID,          // BO ClassID
    Top,              // BOForm
    Left,             // BOForm
    Width,            // BOForm
    Height,           // BOForm
    WindowState,      // BOForm
//    SidePnl,          // MainForm
    SrchPnl,          // MainForm
    PrvPnl: integer;  // MainForm

    procedure Save(aWriter: TWriter);
    procedure Load(aReader: TReader);
  public
    procedure Assign(AInfo: TBOFormInfo);
  end;

  // Forward declarations
  TBusinessObject = class;
  TBOClass = TBusinessObject;
  TBOList = class;

  TAttribute = class
  private
    fIsNull: Boolean;
    fName: String;
    fFldName: string;
    fID: integer;
    fRequiredAtInsert: boolean;
    fRequiredAtFinish: boolean;
    fInQuickSearch: boolean;
    FAttributeChanged: boolean;
    FSize: integer;
    FValueChanged: boolean;
    FOwner : TBusinessObject;
    FOnChange: TNotifyEvent;
    function GetName: String;
    procedure SetName(const Value: String);
    procedure SetRequiredAtInsert(const Value: boolean);
    procedure SetRequiredAtFinish(const Value: boolean);
    procedure SetAttributeChanged(const Value: boolean);
    procedure SetInQuickSearch(const Value: boolean);
    procedure SetID(const Value: integer);
    procedure SetSize(const Value: integer);
    function GetSize: integer; virtual;
    procedure SetValueChanged(const Value: boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetOwner: TBusinessObject;
  public
    constructor Create; overload; virtual;
    function GetMetaType: TMetaType; virtual; abstract;
    function GetMetaTypeName: string;
    function Copy: TAttribute; virtual; abstract;
    function IsNull: Boolean; virtual;
    procedure SetNull; virtual; abstract;
    property Name: String read GetName write SetName;
    property FldName: string read fFldName write fFldName;
    property ID: integer read fID write SetID;
    property Size: integer read GetSize write SetSize;
    property RequiredAtInsert: boolean read fRequiredAtInsert write SetRequiredAtInsert;
    property RequiredAtFinish: boolean read fRequiredAtFinish write SetRequiredAtFinish;
    property InQuickSearch: boolean read fInQuickSearch write SetInQuickSearch;
    property ValueChanged: boolean read FValueChanged write SetValueChanged;
    // Property to indicate that the attribute in the model has changed and has to be updated in the Database
    property AttributeChanged: boolean read fAttributeChanged write SetAttributeChanged;
    property MetaType: TMetaType read GetMetaType;
    property Owner: TBusinessObject read GetOwner write FOwner;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TIDAttribute = class(TAttribute)
  private
    function GetValue: integer;
    procedure SetValue(const Value: integer);
  public
    fValue: integer;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: integer read GetValue write SetValue;
  end;

  TIntegerAttribute = class(TAttribute)
  private
    procedure SetValue(Value: Integer);
    function GetValue: Integer;
  public
    fValue: Integer;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: Integer read GetValue write SetValue;
  end;

  TIntCalcAttribute = class(TAttribute)
  private
    procedure SetValue(Value: Integer);
    function GetValue: Integer;
  public
    fValue: Integer;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: Integer read GetValue write SetValue;
  end;

  TDoubleAttribute = class(TAttribute)
  private
    function GetValue: double;
    procedure SetValue(const Value: double);
  public
    fValue: double;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: double read GetValue write SetValue;
  end;

  TCurrencyAttribute = class(TAttribute)
  private
    function GetValue: Currency;
    procedure SetValue(const Value: Currency);
  public
    fValue: Currency;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: Currency read GetValue write SetValue;
  end;

  TStringAttribute = class(TAttribute)
  private
    fSize: integer;
    procedure SetValue(Value: String);
    function GetValue: String;
    function GetSize: integer; override;
  public
    fValue: String;
    constructor Create(anID: integer; aName, aFldName: String; aSize: integer);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: String read GetValue write SetValue;
    property Size: integer read GetSize write fSize;
  end;

  TStringComboAttribute = class(TAttribute)
  private
    fSize: integer;
    fList: TStringList;
    fListCode: string;
    procedure SetValue(Value: String);
    function GetValue: String;
    procedure SetListCode(const Value: string);
    function GetSize: integer; override;
  public
    fValue: String;
    constructor Create(anID: integer; aName, aFldName: String; aSize: integer; aList: TStringList; aListCode: string);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: String read GetValue write SetValue;
    property Size: integer read GetSize write fSize;
    property List: TStringList read fList write fList;
    property ListCode: string read fListCode write SetListCode;
  end;

  TStringSelectAttribute = class(TAttribute)
  private
    fSize: integer;
    fList: TStringList;
    fListCode: string;
    procedure SetValue(Value: String);
    function GetValue: String;
    procedure SetListCode(const Value: string);
    function GetSize: integer; override;
  public
    fValue: String;
    constructor Create(anID: integer; aName, aFldName: String; aSize: integer; aList: TStringList; aListCode: string);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: String read GetValue write SetValue;
    property Size: integer read GetSize write fSize;
    property List: TStringList read fList write fList;
    property ListCode: string read fListCode write SetListCode;
  end;

  TDateTimeAttribute = class(TAttribute)
  private
    procedure SetValue(Value: TDateTime);
    function GetValue: TDateTime;
  public
    fValue: TDateTime;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: TDateTime read GetValue write SetValue;
  end;

  TDateAttribute = class(TAttribute)
  private
    function GetValue: TDate;
    procedure SetValue(const Value: TDate);
  public
    fValue: TDate;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: TDate read GetValue write SetValue;
  end;

  TTimeAttribute = class(TAttribute)
  private
    function GetValue: TTime;
    procedure SetValue(const Value: TTime);
  public
    fValue: TTime;
    constructor Create(anID: integer; aName, aFldName: String);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: TTime read GetValue write SetValue;
  end;

  TTreeComboNode = class
  private
    fID: integer;
    fParent: integer;
    fText: string;
    FLeft: integer;
    FRight: integer;
  public
    property ID: integer read fID write fID;
    property Parent: integer read fParent write fParent;
    property Text: string read fText write fText;
    property Left: integer read FLeft write FLeft;
    property Right: integer read FRight write FRight;
  end;

  TTreeComboAttribute = class(TAttribute)
  private
    fTreeList: TList;
    FTreeName: string;
    procedure SetValue(const AValue: integer);
    procedure SetTreeName(const Value: string);
    function GetTextFromTreeItem(aTreeItemID: integer;
      var AParentID: integer): string;
  public
    fValue: integer;
    constructor Create(anID: integer; aName, aFldName, aTreeName: String);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    function GetText: string;
    function GetValueText: string;
    property Value: integer read fValue write SetValue;
    property TreeList: TList read fTreeList write fTreeList;
    property TreeName: string read FTreeName write SetTreeName;
  end;

  TMemoAttribute = class(TAttribute)
  private
    function GetValue: string;
    procedure SetValue(const Value: string);
  public
    fValue: string;
    constructor Create(anID: integer; aName, aFldName: string);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: string read GetValue write SetValue;
  end;

  TAssociationAttribute = class(TAttribute)
  private
    fClassID : integer;
    fSearchFlds: string;

  public
    function GetClass: TBusinessObject;

    property ClassID: integer read fClassID write fClassID;
    property SearchFlds: string read fSearchFlds write fSearchFlds;
  end;

  PVariant = ^Variant;
  {
    This is an association attribute specific for 1-n associations.
  }
  TSingleBOAttribute = class(TAssociationAttribute)
  private
    fBO: TBusinessObject;
    fValue: integer;
    fVarValue: Variant;

    procedure SetValue(const Value: integer);
    procedure SetVarValue(const Value: Variant); virtual;

  protected
    function GetDisplayFld: String; virtual; abstract;

  public
    function GetVarValuePtr: Pointer;
    function IsNull: Boolean; override;

    // Properties
    property DisplayFld: string read GetDisplayFld;
    property BO: TBusinessObject read fBO write fBO;
    property Value: integer read fValue write SetValue;
    property VarValue: Variant read fVarValue write SetVarValue;

    property ValuePtr: Pointer read GetVarValuePtr;

  end;

  TCallNumberAttribute = class(TSingleBOAttribute)
  private
    FPrefix : Variant;
    procedure SetPrefix(const Value: string);
    function GetPrefix: string;

  protected
    function GetDisplayFld: String; override;

  public
    constructor Create(anID: integer; aName, aFldName: String); overload;
    destructor Destroy; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    function GetCallNumber: string;

    property Prefix: string read GetPrefix write SetPrefix;
  end;

  TBOAttribute = class(TSingleBOAttribute)
  private
    fFilter: string;

  protected
    function GetDisplayFld: String; override;

  public
    constructor Create(anID: integer; aName, aFldName, aSearchFlds, AFilter: string; aClassID: integer);
    destructor Destroy; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;

    property Filter: string read fFilter write fFilter;
  end;

  TBOSearchAttribute = class(TSingleBOAttribute)
  private
    fFilter: string;
  protected
    function GetDisplayFld: String; override;

  public
    constructor Create(anID: integer; aName, aFldName, aSearchFlds, AFilter: string; aClassID: integer);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Filter: string read fFilter write fFilter;
  end;

  TBOTreeAttribute = class(TSingleBOAttribute)
  private
    fFilter: string;

  protected
    function GetDisplayFld: String; override;

  public
    constructor Create(anID: integer; aName, aFldName, aSearchFlds, aFilter: string; aClassID: integer);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Filter: string read fFilter write fFilter;
  end;

  TBOListAttribute = class(TAssociationAttribute)
  private
    fFldValue: integer;
    function GetValue: TBOList;
    procedure SetValue(const Value: TBOList);
  public
    fValue: TBOList;
    constructor Create(anID: integer; aName, aFldName: string; aClassID: integer);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: TBOList read GetValue write SetValue;
    property FldValue: integer read fFldValue write fFldValue;
  end;

  THyperLinkAttribute = class(TAttribute)
  private
    fSize: integer;
    function GetValue: string;
    procedure SetValue(const Value: string);
    function GetSize: integer; override;
  public
    fValue: string;
    constructor Create(anID: integer; aName, aFldName: string; aSize: integer);
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: string read GetValue write SetValue;
    property Size: integer read GetSize write fSize;
  end;

  TRadioAttribute = class(TAttribute)
  private
    fList: TStringList;
    fLinkTbl: string;
    fLinkFld: string;
    fLinkValue: string;
    function GetValue: integer;
    procedure SetValue(const Value: integer);
  public
    fValue: integer;
    constructor Create(anID: integer; aName, aFldName: string; aList: TStringList);
    destructor Destroy; override;
    function IsNull: Boolean; override;
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    property Value: integer read GetValue write SetValue;
    property List: TStringList read fList;
    property LinkTbl: string read fLinkTbl write fLinkTbl;
    property LinkFld: string read fLinkFld write fLinkFld;
    property LinkValue: string read fLinkValue write fLinkValue;
  end;

  TStateAttribute = class(TAttribute)
  private
    function GetValue: TState;
    procedure SetValue(const Value: TState);
  public
    fValue: TState;
    IsFinished: boolean;
    constructor Create(anID: integer; aName, aFldName: string);
    procedure SetNull; override;
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    function Text: string;
    property Value: TState read GetValue write SetValue;
  end;

  // An unknown or disabled attribute (db field)
  TUnknownAttribute = class(TAttribute)
  private
  public
    constructor Create(anID: integer; aName, aFldName: string);
    function GetMetaType: TMetaType; override;
    function Copy: TAttribute; override;
    procedure SetNull; override;
  end;

  {**********************************************************}

  TAttributeList = class
  private
    fList: TList;
    function GetCount: Integer;
    function GetItem(idx: integer): TAttribute;
    procedure SetItem(idx: integer; const Value: TAttribute);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Attrib: TAttribute);
    procedure AddFirst(const Attrib : TAttribute);
    procedure Del(idx: integer);
    procedure Assign(aList: TAttributeList);
    function Get(FldName: String): TAttribute;
    function GetByID(AnID: integer): TAttribute;
    function IndexOf(anAttr: TAttribute): integer;
    function Save(filename: String): TMemoryStream;
    property Count: Integer read GetCount;
    property List: TList read fList;
    property Item[idx: integer]: TAttribute read GetItem write SetItem; default;
  end;

  {**********************************************************}

  TBusinessObject = class
  private
    fAttributeList: TAttributeList;
    fName: String;
    fTableName: String;
    fLayout: TStringStream;
    fStrQuickSearch: String;
    fFrmWidth: integer;
    fFrmHeight: integer;
    fClassID: integer;
    fIsSelectable: boolean;
    fIsActive: boolean;
    fIsProxy: boolean;
    fSearchLayout: TStringStream;
    fPreviewLayout: TStringStream;
    fPermissions: TPermissions;
    fIsRead: boolean;
    fIsDeleted: boolean;
    fInActionBar: boolean;
    fTemplateID: integer;
    fIsLocallyLocked: boolean;
    fIsRemoteLocked: boolean;
    fBOFrmInfo: TBOFormInfo;
    fLoadLevel: integer;
    fIcon: TPicture;
    fRefCount: integer;
    FWorkflowID: integer;
    FBOChanged: boolean;
    FGridAttributes: TStringList;
    function GetName: String;
    procedure SetName(const Value: String);
    function GetQuickSearch: string;
    procedure SetQuickSearch(const Value: string);
    procedure SetProxy(const Value: boolean);
    procedure SetWorkflowID(const Value: integer);
    procedure SetBOChanged(const Value: boolean);
    function GetGridAttributes: TStringList;
    procedure SetIsLocallyLocked(const Value: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddAttribute(AValue: TAttribute);
    function Assign(aBO: TBusinessObject; ALoadLevel: integer): boolean;
    function CheckBO: integer;
    procedure ClearAttributes;
    function GetAttribute(FldName: string): TAttribute; overload;
    function GetAttribute(mtType: TMetaType): TAttribute; overload;
    function GetAttributeByPath(AnAttrPath: string): TAttribute; overload;
    function GetAttributeByName(aName: string): TAttribute;
    function GetIDAttribute: TIDAttribute;
    function GetState: TState;
    procedure Release;
    procedure RemoveAttribute(idx: integer);
    procedure IncRefCount;
    // Properties
    property AttributeList: TAttributeList read fAttributeList write fAttributeList;
    property Name: String read GetName write SetName;
    property TblName: String read fTableName write fTableName;
    property StrQuickSearch: string read GetQuickSearch write SetQuickSearch;
    property Layout: TStringStream read fLayout;
    property SearchLayout: TStringStream read fSearchLayout;
    property PreviewLayout: TStringStream read fPreviewLayout;
    property FrmWidth: integer read fFrmWidth write fFrmWidth;
    property FrmHeight: integer read fFrmHeight write fFrmHeight;
    property ClassID: integer read fClassID write fClassID;
    property IsActive: boolean read fIsActive write fIsActive;
    property IsSelectable: boolean read fIsSelectable write fIsSelectable;
    property IsRead: boolean read fIsRead write fIsRead;
    property IsDeleted: boolean read fIsDeleted write fIsDeleted;
    property IsProxy: boolean read fIsProxy write SetProxy;
    property Permissions: TPermissions read fPermissions write fPermissions;
    property InActionBar: boolean read fInActionBar write fInActionBar;
    property TemplateID: integer read fTemplateID write fTemplateID;
    property IsLocallyLocked: boolean read fIsLocallyLocked write SetIsLocallyLocked;
    property IsRemoteLocked: boolean read fIsRemoteLocked write fIsRemoteLocked;
    property BOFrmInfo: TBOFormInfo read fBOFrmInfo; // write fBOFrmInfo;
    property LoadLevel: integer read fLoadLevel write fLoadLevel;
    property Icon: TPicture read fIcon write fIcon;
    property BOChanged: boolean read FBOChanged write SetBOChanged;
    property RefCount: integer read fRefCount write fRefCount;
    // WorkflowID is the workflowID in CallBO's and AFaseID in ActionBO !!
    property WorkflowID: integer read FWorkflowID write SetWorkflowID;
    property GridAttributes: TStringList read GetGridAttributes;
  end;

  TBOList = class
  private
    fList: TList;
    fName: string;
    function GetCount: integer;
    function GetItem(idx: integer): TBusinessObject;
    procedure SetItem(idx: integer; const Value: TBusinessObject);
    function GetOpenCount: integer;
    function GetIndex(aBO: TBusinessObject): integer;

  public
    constructor Create;
    destructor Destroy; override;

    //* Empties the list.
    procedure Clear;
    //* Add a new object to the list.
    procedure Add(const aBO: TBusinessObject);
    //* Deletes an object from the list.
    procedure Delete(aBO: TBusinessObject);

    function Get(Name: String): TBusinessObject;
    function GetByID(aClassID: integer): TBusinessObject;
    function GetByTblName(AName: String): TBusinessObject;
    function GetBO(aClassID, anID: integer): TBusinessObject;
    function GetCallBO(aCallNr: integer): TBusinessObject;
    //* Assign all objects from given list to this list
    procedure Assign(aList: TList);

    //* returns the number of objects in this list.
    property Count: integer read GetCount;

    property OpenCount: integer read GetOpenCount;
    property Item[idx: integer]: TBusinessObject read GetItem write SetItem; default;
    property Name: string read fName write fName;
    property List: TList read fList write fList;
  end;

implementation

uses Variants, uBOMgr;

{ TAttribute }

constructor TAttribute.Create;
begin
  FAttributeChanged := False;
  FValueChanged := False;
  FOnChange := nil;
end;

function TAttribute.GetMetaTypeName: string;
begin
  Result := MetaTypeNames[self.GetMetaType];
end;

function TAttribute.GetName: String;
begin
  Result := fName;
end;

function TAttribute.GetOwner: TBusinessObject;
begin
  Result := FOwner;
end;

function TAttribute.GetSize: integer;
begin
  Result := -1;
end;

function TAttribute.IsNull: Boolean;
begin
  Result := True;
end;

procedure TAttribute.SetAttributeChanged(const Value: boolean);
begin
  fAttributeChanged := Value;
end;

procedure TAttribute.SetID(const Value: integer);
begin
  fID := Value;
end;

procedure TAttribute.SetInQuickSearch(const Value: boolean);
begin
  fInQuickSearch := Value;
end;

procedure TAttribute.SetName(const Value: String);
begin
  fName := Value;
end;

procedure TAttribute.SetOnChange(const Value: TNotifyEvent);
begin
  //FOnChange := Value;
end;

procedure TAttribute.SetRequiredAtFinish(const Value: boolean);
begin
  fRequiredAtFinish := Value;
end;

procedure TAttribute.SetRequiredAtInsert(const Value: boolean);
begin
  fRequiredAtInsert := Value;
end;

procedure TAttribute.SetSize(const Value: integer);
begin
  FSize := Value;
end;

procedure TAttribute.SetValueChanged(const Value: boolean);
begin
  if Value and Assigned(FOnChange) then
  begin
  {$IFDEF debug}OutputDebugString(PChar('SetValueChanged: '+Self.Name)); {$ENDIF}
    FOnChange(Self);
  end;
  FValueChanged := Value;
end;

{ TCallNumberAttribute }

function TCallNumberAttribute.Copy: TAttribute;
var
  Attr : TCallNumberAttribute;
begin
  Attr := TCallNumberAttribute.Create(ID, Name, FldName);
  Attr.Value := Self.Value;
  Attr.VarValue := Self.VarValue;
  Attr.FPrefix := Self.FPrefix;
  Attr.fRequiredAtInsert := Self.fRequiredAtInsert;
  Attr.fRequiredAtFinish := Self.fRequiredAtFinish;
  Result := Attr;
  Result.FOnChange := OnChange;
end;

constructor TCallNumberAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  Name := aName;
  FldName := aFldName;
  fID := anID;
  fClassID := cidCall;
  fSearchFlds := 'Call_Number';
  SetNull;
  FPrefix := null;
end;

destructor TCallNumberAttribute.Destroy;
begin
  BO.Release;
  inherited Destroy;
end;

(**
  Returns the call number this attribute refers to.

  If the value for the attribute is null the callnumber will return the
  translated value from SCMTranstale(I18nNew). If a prefix exists for the call
  number "[Prefix]-[CallNumber]" will be returned otherwise only the string
  "[CallNumber]" will be returned.

  @return String value containing the full call number for the current
          business object.
  
**)
function TCallNumberAttribute.GetCallNumber: string;
var
  pfx : String;
begin
  if (IsNull) then
    Result := ''
  else
  begin
    Result := IntToStr(VarValue);
    pfx := GetPrefix;
    if pfx <> '' then
      Result := pfx + '-' + Result;
  end;
end;

function TCallNumberAttribute.GetDisplayFld: String;
begin
  Result := SearchFlds;
end;

function TCallNumberAttribute.GetMetaType: TMetaType;
begin
  Result := mtCallNumber;
end;

(**
  Returns the prefix for a call number.

  If a prefix was not defined this will give back a string containing the
  prefix as specified by the call number mask otherwise the prefix will be
  returned as it was read from the database.

  @return String value containing the call number prefix.
**)
function TCallNumberAttribute.GetPrefix: string;
begin
  try
    if FPrefix = null then
      FPrefix := '';
  except
  end;
  Result := FPrefix;
end;

procedure TCallNumberAttribute.SetNull;
var aLoadLevel: integer;
begin
  if (fBO <> nil) and (fBO.RefCount > 0) then
  begin
    aLoadLevel := fBO.LoadLevel;
    fBO.Release;
    fBO := TBusinessObject.Create;
    fBO.Assign(BOMgr.BOList.GetByID(fClassID), aLoadLevel);
  end;
  fValue := -1;
  fIsNull := True;
end;

procedure TCallNumberAttribute.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

{ TIDAttribute }

function TIDAttribute.Copy: TAttribute;
begin
  Result := TIDAttribute.Create(ID, Name, FldName);
  TIDAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TIDAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  Name := aName;
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TIDAttribute.GetMetaType: TMetaType;
begin
  Result := mtID;
end;

function TIDAttribute.GetValue: integer;
begin
  Result := fValue;
end;

function TIDAttribute.IsNull: Boolean;
begin
  Result := fValue = -1;
end;

procedure TIDAttribute.SetNull;
begin
  fIsNull := True;
  fValue := -1;
end;

procedure TIDAttribute.SetValue(const Value: integer);
begin
  fValue := Value;
end;

{ TIntegerAttribute }

constructor TIntegerAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

procedure TIntegerAttribute.SetValue(Value: Integer);
begin
  fValue := Value;
  fIsNull := False;
end;

function TIntegerAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TIntegerAttribute.SetNull;
begin
  fValue := 0;
  fIsNull := True;
end;

function TIntegerAttribute.GetMetaType: TMetaType;
begin
  Result := mtInteger;
end;

function TIntegerAttribute.GetValue: Integer;
begin
  Result := fValue;
end;

function TIntegerAttribute.Copy: TAttribute;
begin
  Result := TIntegerAttribute.Create(ID, Name, FldName);
  TIntegerAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

{ TDateTimeAttribute }

const
  NullDateTime = 0;//-693594;

function TDateTimeAttribute.Copy: TAttribute;
begin
  Result := TDateTimeAttribute.Create(ID, Name, FldName);
  TDateTimeAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TDateTimeAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TDateTimeAttribute.GetMetaType: TMetaType;
begin
  Result := mtDateTime;
end;

function TDateTimeAttribute.GetValue: TDateTime;
begin
  Result := fValue;
end;

function TDateTimeAttribute.IsNull: Boolean;
begin
  Result := fValue = NullDateTime;
end;

procedure TDateTimeAttribute.SetNull;
begin
  fValue := NullDateTime;
  fIsNull := True;
end;

procedure TDateTimeAttribute.SetValue(Value: TDateTime);
begin
  fValue := Value;
  fIsNull := False;
end;

{ TDateAttribute }

function TDateAttribute.Copy: TAttribute;
begin
  Result := TDateAttribute.Create(ID, Name, FldName);
  TDateAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TDateAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TDateAttribute.GetMetaType: TMetaType;
begin
  Result := mtDate;
end;

function TDateAttribute.GetValue: TDate;
begin
  Result := fValue;
end;

function TDateAttribute.IsNull: Boolean;
begin
  Result := fValue = NullDateTime;
//  Result := fIsNull;
end;

procedure TDateAttribute.SetNull;
begin
  fValue := NullDateTime;
  fIsNull := True;
end;

procedure TDateAttribute.SetValue(const Value: TDate);
begin
  fValue := Value;
  fIsNull := False;
end;

{ TTimeAttribute }

function TTimeAttribute.Copy: TAttribute;
begin
  Result := TTimeAttribute.Create(ID, Name, FldName);
  TTimeAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TTimeAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TTimeAttribute.GetMetaType: TMetaType;
begin
  Result := mtTime;
end;

function TTimeAttribute.GetValue: TTime;
begin
  Result := fValue;
end;

function TTimeAttribute.IsNull: Boolean;
begin
  Result := fValue = NullDateTime;
end;

procedure TTimeAttribute.SetNull;
begin
  fValue := NullDateTime;
end;

procedure TTimeAttribute.SetValue(const Value: TTime);
begin;
  fValue := Value;
end;

{ TStringAttribute }

function TStringAttribute.Copy: TAttribute;
begin
  Result := TStringAttribute.Create(ID, Name, FldName, Size);
  TStringAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TStringAttribute.Create(anID: integer; aName, aFldName: String; aSize: integer);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  Size := aSize;
  SetNull;
  fID := anID;
end;

function TStringAttribute.GetMetaType: TMetaType;
begin
  Result := mtString;
end;

function TStringAttribute.GetSize: integer;
begin
  Result := fSize;
end;

function TStringAttribute.GetValue: String;
begin
  Result := fValue;
end;

function TStringAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TStringAttribute.SetNull;
begin
  fValue := '';
  fIsNull := True;
end;

procedure TStringAttribute.SetValue(Value: String);
begin
  fValue := Value;
  fIsNull := fValue = '';
end;

{ TTreeComboAttribute }

function TTreeComboAttribute.Copy: TAttribute;
begin
  Result := TTreeComboAttribute.Create(fID, fName, fFldName, fTreeName);
  TTreeComboAttribute(Result).TreeList.Assign(fTreeList);
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TTreeComboAttribute.Create(anID: integer; aName,
  aFldName, aTreeName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  fID := anID;
  fTreeList := TList.Create;
  fTreeName := aTreeName;
  SetNull;
end;

destructor TTreeComboAttribute.Destroy;
begin
  if (fTreeList.Count > 0) then
    fTreeList.Clear;
  fTreeList.Free;
  inherited;
end;

function TTreeComboAttribute.GetMetaType: TMetaType;
begin
  Result := mtTreeCombo;
end;

function TTreeComboAttribute.GetText: string;
var s: string;
    iParent: integer;
begin
  iParent := 0;
  Result := GetTextFromTreeItem(fValue, iParent);
  s := '';
  while (iParent <> 0) do
  begin
    s := s + GetTextFromTreeItem(iParent, iParent);
    if (iParent <> 0) then
      s := s + '-';
  end;
  if (s <> '') then
    Result := Result + ' (' + s + ')';
end;

function TTreeComboAttribute.GetTextFromTreeItem(
  aTreeItemID: Integer; var AParentID: integer): string;
var idx: integer;
begin
  Result := '';
  for idx := 0 to pred(fTreeList.Count) do
  begin
    if TTreeComboNode(fTreeList.Items[idx]).ID = aTreeItemID then
    begin
      Result := TTreeComboNode(fTreeList.Items[idx]).Text;
      AParentID := TTreeComboNode(fTreeList.Items[idx]).Parent;
      Break;
    end;
  end;
end;

function TTreeComboAttribute.GetValueText: string;
var i: integer;
begin
  Result := GetTextFromTreeItem(fValue, i);
end;

function TTreeComboAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TTreeComboAttribute.SetNull;
begin
  fValue := -1;
  fIsNull := True;
end;

procedure TTreeComboAttribute.SetTreeName(const Value: string);
begin
  FTreeName := Value;
end;

procedure TTreeComboAttribute.SetValue(const AValue: integer);
begin
  fValue := AValue;
  fIsNull := (fValue = -1);
end;

{ TMemoAttribute }

function TMemoAttribute.Copy: TAttribute;
begin
  Result := TMemoAttribute.Create(ID, Name, FldName);
  TMemoAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TMemoAttribute.Create(anID: integer; aName, aFldName: string);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TMemoAttribute.GetMetaType: TMetaType;
begin
  Result := mtMemo;
end;

function TMemoAttribute.GetValue: string;
begin
  result := fValue;
end;

function TMemoAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TMemoAttribute.SetNull;
begin
  SetValue('');
end;

procedure TMemoAttribute.SetValue(const Value: string);
begin
  fValue := Value;
  fIsNull := Value = '';
end;

{ TBOAttribute }

function TBOAttribute.Copy: TAttribute;
begin
  Result := TBOAttribute.Create(ID, Name, FldName, SearchFlds, Filter, ClassID);
  TBOAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TBOAttribute.Create(anID: integer; aName, aFldName,
  aSearchFlds, AFilter: string; aClassID: integer);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  ClassID := aClassID;
  SearchFlds := aSearchFlds;
  Filter := aFilter;
//  BO := TBusinessObject.Create;
  SetNull;
  fID := anID;
end;

destructor TBOAttribute.Destroy;
begin
  BO.Release;
  inherited Destroy;
end;

function TBOAttribute.GetDisplayFld: String;
begin
  Result := '';
  with TStringList.Create do
  begin
    CommaText := SearchFlds;
    if Count > 0 then
      Result := Strings[0];
    Free;
  end;
end;

function TBOAttribute.GetMetaType: TMetaType;
begin
  Result := mtBOCombo;
end;

procedure TBOAttribute.SetNull;
var aLoadLevel: integer;
begin
//  fBO.ClearAttributes;  // cann't be done, because the bo is not a copy of the 'real' bo
                          // but it is the 'real' bo
  if (fBO <> nil) and (fBO.RefCount > 0) then
  begin
    aLoadLevel := fBO.LoadLevel;
    fBO.Release;
    fBO := TBusinessObject.Create;
    fBO.Assign(BOMgr.BOList.GetByID(fClassID), aLoadLevel);
  end;
  fValue := -1;
  fIsNull := True;
end;

{ TBOSearchAttribute }

function TBOSearchAttribute.Copy: TAttribute;
begin
  Result := TBOSearchAttribute.Create(ID, Name, FldName, SearchFlds, Filter, ClassID);
  TBOSearchAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TBOSearchAttribute.Create(anID: integer; aName, aFldName,
  aSearchFlds, AFilter: string; aClassID: integer);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  ClassID := aClassID;
  Filter := aFilter;
  SearchFlds := aSearchFlds;
//  BO := TBusinessObject.Create;
  SetNull;
  fID := anID;
end;

destructor TBOSearchAttribute.Destroy;
begin
  BO.Release;
  inherited Destroy;
end;

function TBOSearchAttribute.GetDisplayFld: String;
begin
  Result := '';
  with TStringList.Create do
  begin
    CommaText := SearchFlds;
    if Count > 0 then
      result := strings[0];
    Free;
  end;
end;

function TBOSearchAttribute.GetMetaType: TMetaType;
begin
  Result := mtBOSearch;
end;

function TBOSearchAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TBOSearchAttribute.SetNull;
var aLoadLevel: integer;
begin
//  fBO.ClearAttributes;  // cann't be done, because the bo is not a copy of the 'real' bo
                          // but it is the 'real' bo
  if (fBO <> nil) and (fBO.RefCount > 0) then
  begin
    aLoadLevel := fBO.LoadLevel;
    fBO.Release;
    fBO := TBusinessObject.Create;
    fBO.Assign(BOMgr.BOList.GetByID(fClassID), aLoadLevel);
  end;
  fValue := -1;
  fIsNull := True;
end;

{ TBOTreeAttribute }

function TBOTreeAttribute.Copy: TAttribute;
begin
  Result := TBOTreeAttribute.Create(ID, Name, FldName, SearchFlds, Filter, ClassID);
  TBOTreeAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TBOTreeAttribute.Create(anID: integer; aName, aFldName,
  aSearchFlds, aFilter: string; aClassID: integer);
begin
  inherited Create;
  fID := anID;
  SetName(aName);
  FldName := aFldName;
  SearchFlds := aSearchFlds;
  Filter := aFilter;
  ClassID := aClassID;
//  BO := TBusinessObject.Create;
  SetNull;
end;

destructor TBOTreeAttribute.Destroy;
begin
  BO.Release;
  inherited Destroy;
end;

function TBOTreeAttribute.GetDisplayFld: String;
begin
  Result := '';
  with TStringList.Create do
  begin
    CommaText := SearchFlds;
    if Count > 0 then
      Result := strings[Count-1];
    Free;
  end;
end;

function TBOTreeAttribute.GetMetaType: TMetaType;
begin
  Result := mtBOTree;
end;

function TBOTreeAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TBOTreeAttribute.SetNull;
var aLoadLevel: integer;
begin
//  fBO.ClearAttributes;  // cann't be done, because the bo is not a copy of the 'real' bo
                          // but it is the 'real' bo
  if (fBO <> nil) and (fBO.RefCount > 0) then
  begin
    aLoadLevel := fBO.LoadLevel;
    fBO.Release;
    fBO := TBusinessObject.Create;
    fBO.Assign(BOMgr.BOList.GetByID(fClassID), aLoadLevel);
  end;
  fValue := -1;
  fIsNull := True;
end;

{ TBOListAttribute }

function TBOListAttribute.Copy: TAttribute;
var i: integer;
begin
  Result := TBOListAttribute.Create(ID, Name, FldName, ClassID);
  for i := 0 to pred(Value.Count) do
  begin
    TBOListAttribute(Result).Value.Add(Value.Item[i]);
  end;
  Result.FOnChange := OnChange;
end;

constructor TBOListAttribute.Create(anID: integer; aName, aFldName: string; aClassID: integer);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  Value := TBOList.Create;
  Value.Name := FldName;
  FldValue := -1;
  ClassID := aClassID;
  fID := anID;
end;

destructor TBOListAttribute.Destroy;
begin
  fValue.Free;
  inherited Destroy;
end;

function TBOListAttribute.GetMetaType: TMetaType;
begin
  Result := mtBOList;
end;

function TBOListAttribute.GetValue: TBOList;
begin
  Result := fValue;
end;

function TBOListAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TBOListAttribute.SetNull;
begin
  fValue.Clear;
  fFldValue := -1;
  fIsNull := True;
end;

procedure TBOListAttribute.SetValue(const Value: TBOList);
begin
  fValue := Value;
  fIsNull := False;
end;

{ THyperLinkAttribute }

function THyperLinkAttribute.Copy: TAttribute;
begin
  Result := THyperLinkAttribute.Create(ID, Name, FldName, Size);
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor THyperLinkAttribute.Create(anID: integer; aName, aFldName: string; aSize: integer);
begin
  inherited Create;
  Name := aName;
  FldName := aFldName;
  Size := aSize;
  SetNull;
  fID := anID;
end;

function THyperLinkAttribute.GetMetaType: TMetaType;
begin
  Result := mtHyperlink;
end;

function THyperLinkAttribute.GetSize: integer;
begin
  Result := fSize;
end;

function THyperLinkAttribute.GetValue: string;
begin
  Result := fValue;
end;

function THyperLinkAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure THyperLinkAttribute.SetNull;
begin
  Value := '';
  fIsNull := True;
end;

procedure THyperLinkAttribute.SetValue(const Value: string);
begin
  fValue := Value;
  fIsNull := False;
end;

{ TRadioAttribute }

function TRadioAttribute.Copy: TAttribute;
begin
  Result := TRadioAttribute.Create(ID, Name, FldName, List);
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TRadioAttribute.Create(anID: integer; aName, aFldName: string;
  aList: TStringList);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  fList := TStringList.Create;
  fList.Assign(aList);
  SetNull;
  fID := anID;
end;

destructor TRadioAttribute.Destroy;
begin
  fList.Free;
  inherited;
end;

function TRadioAttribute.GetMetaType: TMetaType;
begin
  Result := mtRadio;
end;

function TRadioAttribute.GetValue: integer;
begin
  Result := fValue;
end;

function TRadioAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TRadioAttribute.SetNull;
begin
  fValue := -1;
  fIsNull := True;
end;

procedure TRadioAttribute.SetValue(const Value: integer);
begin
  fValue := Value;
  fIsNull := False;
end;

{ TIntCalcAttribute }

function TIntCalcAttribute.Copy: TAttribute;
begin
  Result := TIntCalcAttribute.Create(ID, Name, FldName);
  TIntCalcAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TIntCalcAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TIntCalcAttribute.GetMetaType: TMetaType;
begin
  Result := mtIntCalc;
end;

function TIntCalcAttribute.GetValue: Integer;
begin
  Result := fValue;
end;

function TIntCalcAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TIntCalcAttribute.SetNull;
begin
  fValue := 0;
  fIsNull := True;
end;

procedure TIntCalcAttribute.SetValue(Value: Integer);
begin
  fValue := Value;
  fIsNull := Value = 0;
end;

{ TDoubleAttribute }

function TDoubleAttribute.Copy: TAttribute;
begin
  Result := TDoubleAttribute.Create(ID, Name, FldName);
  TDoubleAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TDoubleAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TDoubleAttribute.GetMetaType: TMetaType;
begin
  Result := mtDouble;
end;

function TDoubleAttribute.GetValue: double;
begin
  Result := fValue;
end;

function TDoubleAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TDoubleAttribute.SetNull;
begin
  fValue := 0;
  fIsNull := True;
end;

procedure TDoubleAttribute.SetValue(const Value: double);
begin
  fValue := Value;
  fIsNull := False;
end;

{ TCurrencyAttribute }

function TCurrencyAttribute.Copy: TAttribute;
begin
  Result := TCurrencyAttribute.Create(ID, Name, FldName);
  TCurrencyAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TCurrencyAttribute.Create(anID: integer; aName, aFldName: String);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetNull;
  fID := anID;
end;

function TCurrencyAttribute.GetMetaType: TMetaType;
begin
  Result := mtCurrency;
end;

function TCurrencyAttribute.GetValue: Currency;
begin
  Result := fValue;
end;

function TCurrencyAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TCurrencyAttribute.SetNull;
begin
  fValue := 0.00;
  fIsNull := True;
end;

procedure TCurrencyAttribute.SetValue(const Value: Currency);
begin
  fValue := Value;
  fIsNull := Value = 0.00;
end;

{ TStringComboAttribute }

function TStringComboAttribute.Copy: TAttribute;
begin
  Result := TStringComboAttribute.Create(ID, Name, FldName, Size, List, ListCode);
  TStringComboAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TStringComboAttribute.Create(anID: integer; aName, aFldName: string;
  aSize: integer; aList: TStringList; aListCode: string);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  Size := aSize;
  List := TStringList.Create;
  List.Assign(aList);
  fID := anID;
  fListCode := aListCode;
end;

destructor TStringComboAttribute.Destroy;
begin
  fList.Free;
  inherited;
end;

function TStringComboAttribute.GetMetaType: TMetaType;
begin
  Result := mtStringCombo;
end;

function TStringComboAttribute.GetSize: integer;
begin
  Result := fSize;
end;

function TStringComboAttribute.GetValue: String;
begin
  Result := fValue;
end;

function TStringComboAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TStringComboAttribute.SetListCode(const Value: string);
begin
  fListCode := Value;
end;

procedure TStringComboAttribute.SetNull;
begin
  fValue := '';
  fIsNull := True;
end;

procedure TStringComboAttribute.SetValue(Value: String);
begin
  fValue := Value;
  fIsNull := (Value = '');
end;

{ TStringSelectAttribute }

function TStringSelectAttribute.Copy: TAttribute;
begin
  Result := TStringSelectAttribute.Create(ID, Name, FldName, Size, List, ListCode);
  TStringSelectAttribute(Result).Value := Value;
  Result.fRequiredAtInsert := RequiredAtInsert;
  Result.fRequiredAtFinish := RequiredAtFinish;
  Result.FOnChange := OnChange;
end;

constructor TStringSelectAttribute.Create(anID: integer; aName, aFldName: String;
  aSize: integer; aList: TStringList; aListCode: string);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  Size := aSize;
  List := TStringList.Create;
  List.Assign(aList);
  fID := anID;
  fListCode := aListCode;
end;

destructor TStringSelectAttribute.Destroy;
begin
  fList.Free;
  inherited;
end;

function TStringSelectAttribute.GetMetaType: TMetaType;
begin
  Result := mtStringSelect;
end;

function TStringSelectAttribute.GetSize: integer;
begin
  Result := fSize;
end;

function TStringSelectAttribute.GetValue: String;
begin
  Result := fValue;
end;

function TStringSelectAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TStringSelectAttribute.SetListCode(const Value: string);
begin
  fListCode := Value;
end;

procedure TStringSelectAttribute.SetNull;
begin
  fValue := '';
  fIsNull := True;
end;

procedure TStringSelectAttribute.SetValue(Value: String);
begin
  fValue := Value;
  fIsNull := (Value = '');
end;

{ TStateAttribute }

function TStateAttribute.Copy: TAttribute;
begin
  Result := TStateAttribute.Create(ID, Name, FldName);
end;

constructor TStateAttribute.Create(anID: integer; aName, aFldName: string);
begin
  inherited Create;
  SetName(aName);
  FldName := aFldName;
  SetValue(stIsOpen);
  fID := anID;
end;

function TStateAttribute.GetMetaType: TMetaType;
begin
  Result := mtState;
end;

function TStateAttribute.GetValue: TState;
begin
  Result := fValue;
end;

procedure TStateAttribute.SetNull;
begin
//
end;

procedure TStateAttribute.SetValue(const Value: TState);
begin
  {$IFDEF debug}Log.Add('Set State to '+statStr[Value]); {$ENDIF}
  fValue := Value;
  IsFinished := (fValue = stIsClosed) or
                (fValue = stActionIsClosed);
end;

function TStateAttribute.Text: string;
begin
  Result := statStr[Value];
end;

{ TUnknownAttribute }

function TUnknownAttribute.Copy: TAttribute;
begin
  Result := TUnknownAttribute.Create(ID, Name, FldName);
end;

constructor TUnknownAttribute.Create(anID: integer; aName,
  aFldName: string);
begin
  inherited Create;
  fID := anID;
  SetName(aName);
  fFldName := aFldName;
end;

function TUnknownAttribute.GetMetaType: TMetaType;
begin
  Result := mtUnknown;
end;

procedure TUnknownAttribute.SetNull;
begin
  inherited;
end;

{ TAttributeList }

procedure TAttributeList.Clear;
var i: integer;
begin
  for i := pred(fList.Count) downto 0 do
    TAttribute(fList[i]).Free;
  fList.Clear;
end;

constructor TAttributeList.Create;
begin
  fList := TList.Create;
end;

destructor TAttributeList.Destroy;
begin
  if (fList <> nil) then
  begin
    Clear;
    fList.Free;
  end;
  inherited Destroy;
end;

procedure TAttributeList.Add(const Attrib: TAttribute);
begin
  fList.Add(Attrib);
end;

function TAttributeList.Get(FldName: String): TAttribute;
var i: integer;
    aFldName: string;
begin
  Result := nil;
  i := 0;
  while (i < Count) and (Result = nil) do
  begin
    if (TAttribute(fList.Items[i]).FldName = FldName) then
    begin
      Result := fList.Items[i];
    end
    else
    if (pos(TAttribute(fList.Items[i]).FldName, FldName) = 1) then
    begin
      aFldName := copy(FldName, Length(TAttribute(fList.Items[i]).FldName)+2, 255);
      case TAttribute(fList.Items[i]).GetMetaType of
        mtCallNumber:begin
                      if (TCallNumberAttribute(fList.Items[i]).BO <> nil) then
                        Result := TCallNumberAttribute(fList.Items[i]).BO.GetAttribute(aFldName);
                    end;
        mtBOCombo:  begin
                      if (TBOAttribute(fList.Items[i]).BO <> nil) then
                        Result := TBOAttribute(fList.Items[i]).BO.GetAttribute(aFldName);
                    end;
        mtBOSearch: begin
                      if (TBOSearchAttribute(fList.Items[i]).BO <> nil) then
                        Result := TBOSearchAttribute(fList.Items[i]).BO.GetAttribute(aFldName);
                    end;
        mtBOTree:   begin
                      if (TBOTreeAttribute(fList.Items[i]).BO <> nil) then
                        Result := TBOTreeAttribute(fList.Items[i]).BO.GetAttribute(aFldName);
                    end;
      end;
    end;
    inc(i);
  end;
end;

function TAttributeList.GetCount: Integer;
begin
  Assert(fList <> nil);
  Result := fList.Count;
end;

function TAttributeList.GetItem(idx: integer): TAttribute;
begin
  try
    Result := fList.Items[idx];
  except
    Result := nil;
  end;
end;

procedure TAttributeList.SetItem(idx: integer; const Value: TAttribute);
begin
  fList.Items[idx] := Value;
end;

function TAttributeList.Save(filename: String): TMemoryStream;
begin
  Result := nil;
end;

procedure TAttributeList.Del(idx: integer);
begin
  fList.Delete(idx);
end;

procedure TAttributeList.Assign(aList: TAttributeList);
begin
  {$IFDEF debug}Log.Add('TAttributeList - Assign'); {$ENDIF}
  fList.Assign(TList(aList));
end;

function TAttributeList.IndexOf(anAttr: TAttribute): integer;
begin
  Result := fList.IndexOf(anAttr);
end;

function TAttributeList.GetByID(AnID: integer): TAttribute;
var i: integer;
begin
  Result := nil;
  for i := 0 to pred(Count) do
  begin
    if (TAttribute(fList.Items[i]).ID = AnID) then
    begin
      Result := fList.Items[i];
    end;
  end;
end;

{ TBusinessObject }

{*
  Adds an Attribute.

  @note Attributes having meta type mtID or mtCallNumber will be added to the
        front of the list.
}
procedure TBusinessObject.AddAttribute(AValue: TAttribute);
begin
  fAttributeList.Add(AValue);
  AValue.Owner := self;
end;

{
  Assign the attributes of a given business object to self.

  @param aBO source object containing the the attributes we want to assign.
  @param ALoadLevel integer indicating the current load level.

  @return a boolean value indicating success or failure of the assign.
}
function TBusinessObject.Assign(aBO: TBusinessObject; ALoadLevel: integer): boolean;
//var idx: integer;
begin
{  if (aBO = nil) or (aBO.TblName = '') then
  begin
    Result := False;
    Exit;
  end;

  if (aBO <> nil) and (aBO <> self) then
  begin
    Self.IncRefCount;
    Name := aBO.Name;
    TblName := aBO.TblName;
    ClassID := aBO.ClassID;

    // copy the 'normal' attributes
    fFrmWidth := aBO.FrmWidth;
    fFrmHeight := aBO.FrmHeight;
    fBOFrmInfo.Assign(aBO.BOFrmInfo);
    fPermissions := aBO.Permissions;
    fIcon.Assign(aBO.Icon);
    AttributeList.Clear;

    // copy the attributes
    for idx := 0 to pred(aBO.AttributeList.Count) do
    begin
      AddAttribute(aBO.AttributeList.Item[idx].Copy);
      if (ALoadLevel < 2) then
      begin
        case aBO.AttributeList.Item[idx].GetMetaType of
          mtCallNumber:begin
                        if (aBO.ClassID in [cidAction, cidDIS]) then
                        begin
                          TCallNumberAttribute(AttributeList.Item[idx]).BO :=
                            BOMgr.LoadBO(
                              cidCall,
                              TCallNumberAttribute(AttributeList.Item[idx]).Value,
                              ALoadLevel + 1);
                        end;
                      end;
          mtBOCombo:  begin
                        if (TBOAttribute(AttributeList.Item[idx]).Value > -1) then
                          TBOAttribute(AttributeList.Item[idx]).BO :=
                            BOMgr.LoadBO(
                              TBOAttribute(AttributeList.Item[idx]).ClassID,
                              TBOAttribute(AttributeList.Item[idx]).Value,
                              ALoadLevel + 1)
                        else
                        begin
                          TBOAttribute(AttributeList.Item[idx]).BO :=
                            TBusinessObject.Create;
                          TBOAttribute(AttributeList.Item[idx]).BO.Assign(
                            BOMgr.BOList.GetByID(
                              TBOAttribute(aBO.AttributeList.Item[idx]).ClassID),
                              ALoadLevel + 1);
                        end;
                      end;
          mtBOSearch: begin
                        if (TBOSearchAttribute(AttributeList.Item[idx]).Value > -1) then
                          TBOSearchAttribute(AttributeList.Item[idx]).BO :=
                            BOMgr.LoadBO(
                              TBOSearchAttribute(AttributeList.Item[idx]).ClassID,
                              TBOSearchAttribute(AttributeList.Item[idx]).Value,
                              ALoadLevel + 1)
                        else
                        begin
                          TBOSearchAttribute(AttributeList.Item[idx]).BO :=
                            TBusinessObject.Create;
                          TBOSearchAttribute(AttributeList.Item[idx]).BO.Assign(
                            BOMgr.BOList.GetByID(
                              TBOSearchAttribute(aBO.AttributeList.Item[idx]).ClassID),
                              ALoadLevel + 1);
                        end;
                      end;
          mtBOTree:   begin
                        if (TBOTreeAttribute(AttributeList.Item[idx]).Value > -1) then
                          TBOTreeAttribute(AttributeList.Item[idx]).BO :=
                            BOMgr.LoadBO(
                              TBOTreeAttribute(AttributeList.Item[idx]).ClassID,
                              TBOTreeAttribute(AttributeList.Item[idx]).Value,
                              ALoadLevel + 1)
                        else
                        begin
                          TBOTreeAttribute(AttributeList.Item[idx]).BO :=
                            TBusinessObject.Create;
                          TBOTreeAttribute(AttributeList.Item[idx]).BO.Assign(
                            BOMgr.BOList.GetByID(
                              TBOTreeAttribute(aBO.AttributeList.Item[idx]).ClassID),
                              ALoadLevel + 1);
                        end;
                      end;
        end;
      end;
    end;
    // copy the relations
    FldRelations.Assign(aBO.FldRelations);
    Result := True;
  end
  else
    Result := False;
}
  Result := False;
end;

(**

  Check the businessobject for links to the call table and return the count.

  @return  an integer

**)
function TBusinessObject.CheckBO: integer;
var idx: integer;
begin
  Result := 0;
  for idx := 0 to pred(AttributeList.Count) do
  begin
    case AttributeList.Item[idx].GetMetaType of
      mtBOList:   begin
                    if (TBOListAttribute(AttributeList.Item[idx]).ClassID = cidCall ) then
                      Result := TBOListAttribute(AttributeList.Item[idx]).Value.Count;
                  end;
    end;
  end;
end;

procedure TBusinessObject.ClearAttributes;
var idx: integer;
begin
  for idx := 0 to pred(AttributeList.Count) do
  begin
    AttributeList.Item[idx].SetNull;
  end;
end;

constructor TBusinessObject.Create;
begin
  fAttributeList := TAttributeList.Create;

  fLayout := TStringStream.Create('');
  fSearchLayout := TStringStream.Create('');
  fPreviewLayout := TStringStream.Create('');

  fIsSelectable := True;
  fIsActive := True;
  fTemplateID := -1;

  fBOFrmInfo := TBOFormInfo.Create;
  fFrmWidth := 450;
  fFrmHeight := 400;

  fIcon := TPicture.Create;
  fLoadLevel := 99; // Lower level = more data loaded
  fRefCount := 0;
  FWorkflowID := -1;
  FBOChanged := False;
  FGridAttributes := TStringList.Create;
  FGridAttributes.Delimiter := ';';
  fPermissions := [];
end;

(**

  This is the destructor method for the TBusinessObject class.
  if refcount = 0 (Last reverence to this dataobject) then destroy
  else decrease refcount
**)
destructor TBusinessObject.Destroy;
begin

  // @todo DH check if this reference count handling is correct. I am fairly
  // sure is can't work like this. 
  if (RefCount <= 0) then
  begin
    {$IFDEF debug}
    try
      Log.Add('TBusinessObject - Destroy ' + fName
      + ' ID: '+IntToStr(Self.GetIDAttribute.Value), DEBUG_LOG);
    except
    end;
    {$ENDIF}
    fAttributeList.Free;

    // StringStreams
    fLayout.Free;
    fSearchLayout.Free;
    fPreviewLayout.Free;

    fBOFrmInfo.Free;
    fIcon.Free;
    FGridAttributes.Free;
    inherited Destroy;
  end;
end;

function TBusinessObject.GetAttribute(FldName: String): TAttribute;
begin
  Result := fAttributeList.Get(FldName);
end;

function TBusinessObject.GetAttribute(mtType: TMetaType): TAttribute;
var idx: integer;
begin
  Result := nil;
  try
    for idx := 0 to pred(AttributeList.Count) do
    begin
      if (AttributeList.Item[idx].GetMetaType = mtType) then
      begin
        Result := AttributeList.Item[idx];
        Break;
      end;
    end;
  except
  end;
end;

function TBusinessObject.GetAttributeByPath(AnAttrPath: string): TAttribute;
var Attrs: TStringList;
    Attr: TAttribute;
    tmpBO: TBusinessObject;
    i: integer;
begin
  Attr := nil;
  try
    Attrs := TStringList.Create;
    try
      tmpBO := Self;
      Attrs.Delimiter := '.';
      Attrs.DelimitedText := AnAttrPath;
      for i := 1 to pred(Attrs.Count) do
      begin
        Attr := tmpBO.GetAttribute(Attrs[i]);
        if (Attr is TSingleBOAttribute) then
        begin
          tmpBO := TSingleBOAttribute(Attr).BO;
        end;
      end;
    finally
      Attrs.Free;
    end;
  except
  end;
  Result := Attr;
end;

function TBusinessObject.GetAttributeByName(aName: string): TAttribute;
var idx: integer;
begin
  Result := nil;
  try
    for idx := 0 to pred(AttributeList.Count) do
    begin
      if (AttributeList.Item[idx].Name = aName) then
      begin
        Result := AttributeList.Item[idx];
        Break;
      end;
    end;
  except
  end;
end;

{*
  Returns a list of attributes which will be shown in the list.
}
function TBusinessObject.GetGridAttributes: TStringList;
var
  I : Integer;
  Attr : TAttribute;
  s : String;
begin
  if FGridAttributes.Count = 0 then
    FGridAttributes.DelimitedText := BOMgr.LoadGridColumnDefs(Self.ClassID);

  if FGridAttributes.Count = 0 then
  begin
    for I := 0 to AttributeList.Count-1 do
    begin
      Attr := AttributeList[I];
      if not ({(Attr is TAssociationAttribute)
        or} (Attr.MetaType in [mtUnknown, mtBOList])) then
      begin
        FGridAttributes.Add('#' + IntToStr(Self.ClassID) + '.' + Attr.FldName);
      end;
    end;
  end;

  // Some attributes must be available
  if (Self.ClassID in [cidCall, cidAction]) then
  begin
    s := '#' + IntToStr(Self.ClassID) + '.DatFin';
    if (FGridAttributes.IndexOf(s) = -1) then
      FGridAttributes.Add(s);
  end;

  // ID attribute MUST be available.
  Assert(Self.GetIDAttribute <> nil);
  s := '#' + IntToStr(Self.ClassID) + '.' + Self.GetIDAttribute.FldName;
  if (FGridAttributes.IndexOf(s) = -1) then
    FGridAttributes.Add(s);

  Result := FGridAttributes;
end;

function TBusinessObject.GetIDAttribute: TIDAttribute;
var idx: integer;
begin
  Result := nil;
  for idx := 0 to pred(AttributeList.Count) do
  begin
    case AttributeList.Item[idx].GetMetaType of
      mtID: begin
              Result := TIDAttribute(AttributeList.Item[idx]);
              Break;
            end;
    end;
  end;
end;

function TBusinessObject.GetName: String;
begin
  Result := fName;
end;

function TBusinessObject.GetQuickSearch: string;
begin
  Result := fStrQuickSearch;
end;

function TBusinessObject.GetState: TState;
var idx: integer;
begin
  Result := stIsOpen;
  for idx := 0 to pred(AttributeList.Count) do
  begin
    case AttributeList.Item[idx].GetMetaType of
      mtState:begin
                Result := TStateAttribute(AttributeList.Item[idx]).Value;
                Break;
              end;
    end;
  end;
end;

procedure TBusinessObject.IncRefCount;
begin
  inc(fRefCount);
end;

procedure TBusinessObject.Release;
begin
  if (Self <> nil) then
  begin
    dec(Self.fRefCount);
    if (Self.RefCount <= 0) then
    begin
      Self.Destroy;
    end;
  end;
end;

procedure TBusinessObject.RemoveAttribute(idx: integer);
begin
  fAttributeList.Del(idx);
end;

procedure TBusinessObject.SetBOChanged(const Value: boolean);
begin
  FBOChanged := Value;
end;

procedure TBusinessObject.SetName(const Value: String);
begin
  fName := Value;
end;

(**

  This is a setter method for the Proxy property.
  If the BO is already completly loaded then the proxy property is False

  @param   Value as a boolean constant

**)
procedure TBusinessObject.SetProxy(const Value: boolean);
begin
  if fIsProxy then
    fIsProxy := Value;
end;

procedure TBusinessObject.SetQuickSearch(const Value: string);
begin
  fStrQuickSearch := Value;
end;

procedure TBusinessObject.SetWorkflowID(const Value: integer);
begin
  FWorkflowID := Value;
end;

procedure TAttributeList.AddFirst(const Attrib: TAttribute);
begin
  fList.Insert(0, Attrib);
end;

{ TBOList }

procedure TBOList.Add(const aBO: TBusinessObject);
begin
  fList.Insert(fList.Count,aBO);
  aBO.IncRefCount;
end;

procedure TBOList.Assign(aList: TList);
begin
  fList.Assign(aList);
end;

constructor TBOList.Create;
begin
  fList := TList.Create;
end;

destructor TBOList.Destroy;
begin
  if (fList <> nil) then
  begin
    Clear;
    fList.Free;
  end;
  inherited Destroy;
end;

procedure TBOList.Clear;
var i: integer;
begin
  for i := pred(fList.Count) downto 0 do
  begin
    TBusinessObject(fList[i]).Release;
  end;
  fList.Clear;
end;

procedure TBOList.Delete(aBO: TBusinessObject);
var
  Idx : Integer;
begin
  Idx := GetIndex(aBO);
  if (Idx <> -1) then
    fList.Delete(Idx);
end;

function TBOList.Get(Name: String): TBusinessObject;
var idx: integer;
begin
  Result := nil;
  for idx := 0 to pred(fList.Count) do
  begin
    if (TBusinessObject(fList.Items[idx]).GetName = Name) then
    begin
      Result := fList.Items[idx];
      Break;
    end;
  end;
end;

function TBOList.GetBO(aClassID, anID: integer): TBusinessObject;
var idx: integer;
begin
  Result := nil;
  for idx := 0 to pred(fList.Count) do
  begin
    if (TBusinessObject(fList.Items[idx]).ClassID = aClassID) and
      (TBusinessObject(fList.Items[idx]).GetIDAttribute.Value = anID) then
    begin
      Result := fList.Items[idx];
      Break;
    end;
  end;
end;

function TBOList.GetByID(aClassID: integer): TBusinessObject;
var idx: integer;
begin
  Result := nil;
  for idx := 0 to pred(fList.Count) do
  begin
    if (TBusinessObject(fList.Items[idx]).ClassID = aClassID) then
    begin
      Result := fList.Items[idx];
      Break;
    end;
  end;
end;


function TBOList.GetByTblName(AName: String): TBusinessObject;
var idx: integer;
begin
  Result := nil;
  for idx := 0 to pred(fList.Count) do
  begin
    if (TBusinessObject(fList.Items[idx]).TblName = AName) then
    begin
      Result := fList.Items[idx];
      Break;
    end;
  end;
end;


{
  Get a business object by it's call number.

  @param aCallNr integer containing a call number.

  @return The business object having the requested call number.
}
function TBOList.GetCallBO(aCallNr: integer): TBusinessObject;
var idx: integer;
begin
  Result := nil;
  for idx := 0 to pred(fList.Count) do
  begin
    if (TBusinessObject(fList.Items[idx]).ClassID = cidCall) and
      (TCallNumberAttribute(TBusinessObject(fList.Items[idx]).GetAttribute('Call_Number')).Value = aCallNr) then
    begin
      Result := fList.Items[idx];
      Break;
    end;
  end;
end;

function TBOList.GetCount: integer;
begin
  Assert(fList <> nil);
  try
    Result := fList.Count;
  except
    Result := -1;
  end;
end;

function TBOList.GetIndex(aBO: TBusinessObject): integer;
var
  idx: integer;
begin
  Result := -1;
  for idx := 0 to pred(Count) do
  begin
    if (Item[idx].GetIDAttribute.Value = aBO.GetIDAttribute.Value) then
    begin
      Result := idx;
      Break;
    end;
  end;
end;

function TBOList.GetItem(idx: integer): TBusinessObject;
begin
  Result := fList.Items[idx];
end;

function TBOList.GetOpenCount: integer;
var idx: integer;
begin
  Result := 0;
  for idx := 0 to pred(Count) do
  begin
    if not (Item[idx].GetState in [stActionIsClosed, stIsClosed]) then
      inc(Result);
  end;
end;

procedure TBOList.SetItem(idx: integer; const Value: TBusinessObject);
begin
  fList.Items[idx] := Value;
end;

{ TBOFormInfo }

procedure TBOFormInfo.Assign(AInfo: TBOFormInfo);
begin
  Top := AInfo.Top;
  Left := AInfo.Left;
  Width := AInfo.Width;
  Height := AInfo.Height;
  WindowState := AInfo.WindowState;
//  SidePnl := AInfo.SidePnl;
  SrchPnl := AInfo.SrchPnl;
  PrvPnl := AInfo.PrvPnl;
end;

procedure TBOFormInfo.Load(aReader: TReader);
begin
  Top := aReader.ReadInteger;
  Left := aReader.ReadInteger;
  Width := aReader.ReadInteger;
  Height := aReader.ReadInteger;
  WindowState := aReader.ReadInteger;
//  SidePnl := aReader.ReadInteger;
  SrchPnl := aReader.ReadInteger;
  PrvPnl := aReader.ReadInteger;
end;

procedure TBOFormInfo.Save(aWriter: TWriter);
begin
  aWriter.WriteInteger(Top);
  aWriter.WriteInteger(Left);
  aWriter.WriteInteger(Width);
  aWriter.WriteInteger(Height);
  aWriter.WriteInteger(WindowState);
//  aWriter.WriteInteger(SidePnl);
  aWriter.WriteInteger(SrchPnl);
  aWriter.WriteInteger(PrvPnl);
end;

{ TAssociationAttribute }

function TAssociationAttribute.GetClass: TBusinessObject;
begin
  Result := BOMgr.BOList.GetByID(fClassID);
end;

{ TSingleBOAttribute }

function TSingleBOAttribute.GetVarValuePtr: Pointer;
begin
  Result := @fVarValue;
end;

function TSingleBOAttribute.IsNull: Boolean;
begin
  Result := fIsNull;
end;

procedure TSingleBOAttribute.SetValue(const Value: integer);
begin
  fValue := Value;
  fIsNull := (fValue = -1);
end;

procedure TSingleBOAttribute.SetVarValue(const Value: Variant);
begin
  fVarValue := Value;
end;

procedure TBusinessObject.SetIsLocallyLocked(const Value: boolean);
begin
  fIsLocallyLocked := Value;
end;

end.
