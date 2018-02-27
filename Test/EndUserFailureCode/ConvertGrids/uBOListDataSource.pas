unit uBOListDataSource;

interface

uses
  Windows, SysUtils, uAttribute, cxCustomData, cxDataStorage, cxGrid, Dialogs,
  cxGridCustomTableView, cxGridTableView, cxGridLevel, cxGraphics, cxCheckBox,
  cxGridCustomView, cxGridDBTableView, Variants, cxCurrencyEdit, Classes, DB,
  Controls, uSQL, uSCMConst, cxMaskEdit;

const
  StateStrCol=-1;

type
  TBOListDataSource = class(TcxCustomDataSource)
  private
    fMainBOList: TBOList;
    fBOListName: string;
    FOwner: TComponent;
    function GetDataBinding(AItemIndex: Integer): TcxGridItemDataBinding;
    procedure SetOwner(const Value: TComponent);
    procedure SetBOListName(const Value: string);
  protected
    function GetInfoForCompare(ARecordHandle: TcxDataRecordHandle;
        AItemHandle: TcxDataItemHandle; var PValueBuffer: PChar): Boolean; override;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
    function IsNativeCompare: Boolean; override;
  public
    constructor Create(aBOList: TBOList);
    destructor Destroy; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetBO(aRecordIndex: integer): TBusinessObject;
    property MainBOList: TBOList read fMainBOList;
    property BOListName: string read fBOListName write SetBOListName;
    property Owner: TComponent read FOwner write SetOwner;
  end;

  TBOListDetailDataSource = class(TcxCustomDataSource)
  private
    fBOListName: string;
    fMyMainBOListDataSource: TBOListDataSource;
    function GetDataBinding(AItemIndex: Integer): TcxGridItemDataBinding;
  protected
    fBOList: TBOList;
    function GetInfoForCompare(ARecordHandle: TcxDataRecordHandle;
        AItemHandle: TcxDataItemHandle; var PValueBuffer: PChar): Boolean; override;
    function GetItemHandle(AItemIndex: Integer): TcxDataItemHandle; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
    function IsNativeCompare: Boolean; override;
  public
    constructor Create(aBOList: TBOList; aBOListDataSource: TBOListDataSource);
    destructor Destroy; override;
    function GetMasterRecordIndex: integer;
    function GetBO(aRecordIndex: integer): TBusinessObject;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    property BOListName: string read fBOListName write fBOListName;
    property MyMainBOListDataSource: TBOListDataSource read fMyMainBOListDataSource;
    property BOList: TBOList read fBOList;
  end;

  TCustomColumnHandler = class(TObject)
  protected
    procedure DoCalcFields(Sender: TDataSet);
    procedure DoActionCalcFields(Sender: TDataSet);
    procedure DoCustomDrawCell(
      Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
        AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
  public
    class function CalculateStatus(
        AClassID: integer; ADatFin, ADatFup: Variant): TState;
  end;

function CreateView(aGrid: TcxGrid; aLevel: TcxGridLevel;
    oClass, oMasterViewClass: TBusinessObject; oAttr: TBOListAttribute;
    aViewNm: string; bAddSublevels: boolean; bIsSearch: boolean = False;
    oCtrl: TControl = nil): TcxGridDBTableView;
function FillGrid(aClassID: integer; aGrid: TcxGrid; aViewNm: string;
  stSQL: string; bAddSublevels: boolean = True; bIsSearch: boolean = False;
  oCtrl: TControl = nil): TcxCustomGridView; overload;

// returns the pointer to the var values location in memory.
function GetValueBufferFromAttribute(anAttribute: TAttribute): PChar;

procedure SetViewProperties(AView: TcxGridDBTableView;
  AClass, AParent: TBusinessObject;
  AAttr: TBOListAttribute; AQuery: TSelectQuery);

implementation

uses
  uBOMgr, Forms, ADODB, DateUtils, cxTextEdit, IniFiles, StrUtils, Unit1,
  uClassMgr;

function CreateCalculatedField(ADataSet : TDataSet; FieldName : string; AClass : TFieldClass) : TField;
var
  i : Integer;
  WorkField : TField;
begin
  {$IFDEF debug}Log.Add('DataMod.CreateCalculatedField'); {$ENDIF}
  ADataSet.FieldDefs.Update;
  for i := 0 to ADataSet.FieldDefs.Count - 1 do
    if ADataSet.FindField(ADataSet.FieldDefs[i].Name) = nil then
      ADataSet.FieldDefs.Items[i].CreateField(ADataSet);
  WorkField := AClass.Create(ADataSet);
  WorkField.Name := ADataSet.Name+FieldName;
  WorkField.FieldName := FieldName;
  WorkField.DisplayLabel := FieldName;
  WorkField.Calculated := True;
  WorkField.FieldKind := fkCalculated;
  WorkField.DataSet := ADataSet;
  Result := WorkField;
  {$IFDEF debug}Log.Add('DataMod.CreateCalculatedField - End'); {$ENDIF}
end;

{ TBOListDataSource }

constructor TBOListDataSource.Create(aBOList: TBOList);
begin
  inherited Create;
  fMainBOList := aBOList;
  BOListName := aBOList.Name;
end;

procedure TBOListDataSource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
begin
end;

destructor TBOListDataSource.Destroy;
begin
  inherited Destroy;
end;

function TBOListDataSource.GetBO(aRecordIndex: integer): TBusinessObject;
begin
  Result := MainBOList.Item[aRecordIndex];
end;

function TBOListDataSource.GetDataBinding(AItemIndex: Integer): TcxGridItemDataBinding;
begin
  Result := TcxCustomGridTableItem(DataController.GetItem(AItemIndex)).DataBinding;
end;

function TBOListDataSource.GetInfoForCompare(ARecordHandle: TcxDataRecordHandle;
                                           AItemHandle: TcxDataItemHandle;
                                           var PValueBuffer: PChar): Boolean;
var
  ADataBinding: TcxGridItemDataBinding;
begin
  ADataBinding := TcxGridItemDataBinding(AItemHandle);
  with MainBOList.Item[Integer(ARecordHandle)].AttributeList do
  begin
    PValueBuffer := GetValueBufferFromAttribute(Item[Integer(ADataBinding.Data)]);
  end;
  Result := PValueBuffer <> nil;
end;

function TBOListDataSource.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
begin
  Result := TcxDataItemHandle(GetDataBinding(AItemIndex));
end;

function TBOListDataSource.GetRecordCount: Integer;
begin
  Result := MainBOList.Count;
end;

function TBOListDataSource.GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(ARecordIndex);
end;

function TBOListDataSource.GetValue(aRecordHandle: TcxDataRecordHandle;
                                  aItemHandle: TcxDataItemHandle): Variant;
var
  aRecordIndex: integer;
  aDataBinding: TcxGridItemDataBinding;
begin
  aDataBinding := TcxGridItemDataBinding(aItemHandle);
  aRecordIndex := integer(aRecordHandle);
  if (MainBOList.List <> nil) and (MainBOList.Count > 0) then
  begin
    with MainBOList.Item[aRecordIndex] do
    begin
      if (integer(aDataBinding.Data)=StateStrCol) then
      begin
        Result := StatStr[TStateAttribute(GetAttribute(mtState)).Value];
      end
      else
      begin
//        Result := GetValueFromAttribute(TAttribute(AttributeList.Item[integer(aDataBinding.Data)]));
      end;
    end;
  end;
end;

function TBOListDataSource.IsNativeCompare: Boolean;
begin
  Result := True;
end;

procedure TBOListDataSource.SetBOListName(const Value: string);
begin
  fBOListName := Value;
end;

procedure TBOListDataSource.SetOwner(const Value: TComponent);
begin
  FOwner := Value;
end;

procedure TBOListDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
begin
end;

{ TBOListDetailDataSource }

constructor TBOListDetailDataSource.Create(aBOList: TBOList; aBOListDataSource: TBOListDataSource);
begin
  inherited Create;
  fBOList := aBOList;
  BOListName := aBOList.Name;
  fMyMainBOListDataSource := aBOListDataSource;
end;

destructor TBOListDetailDataSource.Destroy;
begin
  inherited Destroy;
end;

procedure TBOListDetailDataSource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
begin
end;

function TBOListDetailDataSource.GetBO(
  aRecordIndex: integer): TBusinessObject;
var aBOList : TBOList;
begin
  aBOList := TBOListAttribute(MyMainBOListDataSource.MainBOList.Item[GetMasterRecordIndex].AttributeList.Get(BOListName)).Value;
  Result := aBOList.Item[aRecordIndex];
end;

function TBOListDetailDataSource.GetDataBinding(
  AItemIndex: Integer): TcxGridItemDataBinding;
begin
  Result := TcxCustomGridTableItem(DataController.GetItem(AItemIndex)).DataBinding;
end;

function TBOListDetailDataSource.GetInfoForCompare(ARecordHandle: TcxDataRecordHandle;
                                                    AItemHandle: TcxDataItemHandle;
                                                    var PValueBuffer: PChar): Boolean;
var ADataBinding: TcxGridItemDataBinding;
    aBOList: TBOList;
begin
  ADataBinding := TcxGridItemDataBinding(AItemHandle);
  aBOList := TBOListAttribute(MyMainBOListDataSource.MainBOList.Item[GetMasterRecordIndex].AttributeList.Get(BOListName)).Value;
  with aBOList.Item[Integer(ARecordHandle)].AttributeList do
  begin
    PValueBuffer := GetValueBufferFromAttribute(TAttribute(Item[integer(aDataBinding.Data)]));
  end;
  Result := PValueBuffer <> nil;
end;

function TBOListDetailDataSource.GetItemHandle(AItemIndex: Integer): TcxDataItemHandle;
begin
  Result := TcxDataItemHandle(GetDataBinding(AItemIndex));
end;

function TBOListDetailDataSource.GetMasterRecordIndex: integer;
begin
  Result := DataController.GetMasterRecordIndex;
end;

function TBOListDetailDataSource.GetRecordCount: Integer;
begin
  Result := 0;
  if (GetMasterRecordIndex >= 0) and
   (MyMainBOListDataSource.MainBOList.Item[GetMasterRecordIndex].GetAttribute(BOListName) <> nil) then
  begin
    Result := TBOListAttribute(MyMainBOListDataSource.MainBOList.Item[GetMasterRecordIndex].GetAttribute(BOListName)).Value.Count;
  end;
end;

function TBOListDetailDataSource.GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle;
begin
  Result := TcxDataRecordHandle(ARecordIndex);
end;

function TBOListDetailDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var aBOList: TBOList;
    aDataBinding: TcxGridItemDataBinding;
    aRecordIndex: integer;
begin
  aDataBinding := TcxGridItemDataBinding(AItemHandle);
  aRecordIndex := integer(aRecordHandle);
  aBOList := TBOListAttribute(MyMainBOListDataSource.MainBOList.Item[GetMasterRecordIndex].AttributeList.Get(BOListName)).Value;
  with aBOList.Item[aRecordIndex] do
  begin
    if (integer(aDataBinding.Data)=StateStrCol) then
    begin
      Result := StatStr[TStateAttribute(GetAttribute(mtState)).Value];
    end
    else
    begin
//      Result := GetValueFromAttribute(TAttribute(
//                  AttributeList.Item[integer(aDataBinding.Data)]));
    end;
  end;
end;

procedure TBOListDetailDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
begin
end;

function TBOListDetailDataSource.IsNativeCompare: Boolean;
begin
  Result := True;
end;

{ routines }

{
  Connect the events of the form on the new view.

  @param aGrid the grid the view was created for.
  @param aDetailView a TcxGridTableView object
  @param aClassID integer containing the class id of the elements which will
         be shown in the grid.
}
procedure ConnectGridEvents(aGrid: TcxGrid; aView: TcxGridTableView;
    aClassID: Integer);
begin
  if (aGrid.Name = 'MainGrid') then
  begin
    with aView do
    begin
      OnGetStoredProperties := FrmMain.OnGetStoredProperties;
      OnGetStoredPropertyValue := FrmMain.OnGetStoredPropertyValue;
      OnSetStoredPropertyValue := FrmMain.OnSetStoredPropertyValue;
    end;
  end;
end;

{
  Connect a view onto a grid on specified level.

  @param aGrid a TcxGrid we are working on
  @param aLevel The level to add a view onto
  @param aView a TcxGridTableView to be connected to the grid level

  @todo cleanup and rename aDetailView to aView
}
procedure ConnectViewToLevel(aGrid : TcxGrid; aLevel: TcxGridLevel;
                            aView: TcxGridTableView);
begin
  // Connect the view to the current level
  if (aGrid.Levels.Count > 0) then
  begin
    {$IFDEF debug}
      Log.Add('Add Gridlevel: ' + aView.Name +
          ' to level: '+aLevel.Items[(aGrid.Levels.Count-1)].GridView.Name, DEBUG_LOG);
    {$ENDIF}
    aLevel.Items[(aGrid.Levels.Count-1)].Add.GridView := aView;
  end
  else
  begin
    {$IFDEF debug}
      Log.Add('Add Gridlevel: '+aView.Name);
    {$ENDIF}
    aLevel.Add.GridView := aView;
  end;
end;


function DaysBetweenEx(ADate, ADateFup: TDateTime) : integer;
var
  nDays,
  nWeeks,
  s,
  e : Integer;
begin
  nDays := DaysBetween(ADate, ADateFup);
  nWeeks := WeeksBetween(ADate, ADateFup);
  Result := (nDays - (nWeeks * 2));
  nDays := nDays - (nWeeks * 7);
  Assert((nDays >= 0) and (nDays <= 6));
  if nDays > 0 then
  begin
    s := DayOfTheWeek(ADate);
    e := s + nDays;
    if (s <= 6) and (6 <= e) then
      Dec(Result);
    if (s <= 7) and (7 <= e) then
      Dec(Result);
  end;
end;


{
  Calculate the status of the record (business object)

  @param AClassID integer containing the class id. Needed to identify what
         kind of state should be calculated.
  @param ADatFin variant containing null or date finish
  @param ADatFup variant contaiing null or date follow-up

  @return TState containing the calculated state from the given parameters.
}
class function TCustomColumnHandler.CalculateStatus(AClassID: integer;
    ADatFin, ADatFup: Variant): TState;
var
  aDT : TDate;
  dtFup : TDateTime;
  aDaysBetween: integer;
begin
  Result := stIsOpen;
  aDT := Now;
  dtFup := 0;
  try
    if not (ADatFup = null) then
      dtFup := StrToDateTime(ADatFup);
  except
  end;

  if (dtFup = 0) then
    aDaysBetween := 0
  else
  begin
    aDaysBetween := DaysBetweenEx(aDT, dtFup);
  end;
  if (AClassID = cidCall) then
  begin
    if not (ADatFin = null) then
      Result := stIsClosed
    else
    if (ADatFup = null) then
      Result := stIsOpen
    else
    if (dtFup < aDT) and (dtFup > 0) then
      Result := stIsOverDue
    else
    if (aDaysBetween = 0)
      and (DayOfWeek(dtFup) =
            DayOfWeek(aDT)) then
      Result := stIsDueToday
    else
    if (1 <= aDaysBetween) and
        (aDaysBetween <= 3) then
      Result := stIsWarning
    else
      Result := stIsOpen;
  end
  else
  if (AClassID = cidAction) then
  begin
    if not (ADatFin = null) then
      Result := stActionIsClosed
    else
    if (ADatFup = null) then
      Result := stActionIsOpen
    else
    if (dtFup < aDT) and (dtFup > 0) then
      Result := stActionIsOverDue
    else
    if (aDaysBetween = 0)
      and (DayOfWeek(dtFup) =
            DayOfWeek(aDT)) then
      Result := stActionIsDueToday
    else
    if (aDaysBetween <= 3) then
      Result := stActionIsWarning
    else
      Result := stActionIsOpen;
  end;
end;

// Handler for the calculated fields.
procedure TCustomColumnHandler.DoActionCalcFields(Sender: TDataSet);
var
  FldDatFin,
  FldDatFup : TFieldDef;
  DatFin,
  DatFup  : Variant;
  sState : string;
  Status : TState;
begin
  try
    FldDatFin := Sender.FieldDefs.Find('base_DatFin');
    FldDatFup := Sender.FieldDefs.Find('base_DatFup');

    DatFin := Sender.Fields[FldDatFin.Index].Value;
    DatFup := Sender.Fields[FldDatFup.Index].Value;

    Status := CalculateStatus(cidAction,DatFin, DatFup);
    sState := statStr[Status];
    Sender.Fields.FindField('State').Value := sState;
    Sender.Fields.FindField('StateCheckBox').Value := (Status = stIsClosed) or
                                                      (Status = stActionIsClosed);
  except
  end;
end;

procedure TCustomColumnHandler.DoCalcFields(Sender: TDataset);
var FldDatFin,
    FldDatFup : TFieldDef;
    DatFin,
    DatFup : Variant;
    sState : string;
    Status : TState;
begin
  try
    FldDatFin := Sender.FieldDefs.Find('base_DatFin');
    FldDatFup := Sender.FieldDefs.Find('base_DatFup');

    DatFin := Sender.Fields[FldDatFin.Index].Value;
    DatFup := Sender.Fields[FldDatFup.Index].Value;

    Status := CalculateStatus(cidCall,DatFin, DatFup);

    if not VarIsNull(Sender.FieldValues['Call_OnHold']) then
      Status := stOnHold;

    sState := statStr[Status];
    Sender.Fields.FindField('State').Value := sState;
    Sender.Fields.FindField('StateCheckBox').Value := (Status = stIsClosed) or
                                                      (Status = stActionIsClosed);
  except
  end;

end;

{
  Sets the Properties for the new view according to the business object model.

  @param oView TcxGridDBTableView object containing the newly created view for
         which the properties schould be created.
  @param oClass TBusinessObject containing the parent class used for data
         generation
}
procedure SetViewProperties(AView: TcxGridDBTableView;
  AClass, AParent: TBusinessObject;
  AAttr: TBOListAttribute; AQuery: TSelectQuery);
var oDS : TDataSet;
    oDSrc : TDataSource;
begin
  with AView.DataController do
  begin
    KeyFieldNames := 'base_' + AClass.GetIDAttribute.FldName;
    if AAttr <> nil then
    begin
      MasterKeyFieldNames := 'base_' + AParent.GetIDAttribute.FldName;
      DetailKeyFieldNames := 'base_' + AAttr.FldName;
      AQuery.AddOrder(AQuery.BaseTable, DetailKeyFieldNames, ASCENDING);
    end;

    // Init the datasource
    oDS := FrmMain.GetSearchDataSet(AQuery.ToString, True);
    if (AClass.ClassID = cidCall) then
    begin
      CreateCalculatedField(oDS, 'State', TStringField);
      CreateCalculatedField(oDS, 'StateCheckBox', TBooleanField);
      oDS.OnCalcFields := TCustomColumnHandler.Create().DoCalcFields;
    end
    else if (AClass.ClassID = cidAction) then
    begin
      CreateCalculatedField(oDS, 'State', TStringField);
      CreateCalculatedField(oDS, 'StateCheckBox', TBooleanField);
      oDS.OnCalcFields := TCustomColumnHandler.Create().DoActionCalcFields;
    end;
    oDSrc := TDataSource.Create(Application);
    oDSrc.DataSet := oDS;
    DataSource := oDSrc;
    oDS.Open;
  end;
end;

procedure AddRequiredColumns(AClass: TBOClass; AQuery: TSelectQuery);
var
  IDFieldName: String;
begin
  // Check if we need to add the default required attributes
  // only when we are adding attributes for the base class.
  IDFieldName := AClass.GetIDAttribute.FldName;
  if (AQuery.FindCOlumnByAlias('base_' + IDFieldName) = nil) then
    AQuery.AddColumn(AQuery.BaseTable, IDFieldName, 'base_' + IDFieldName);

  if (AClass.ClassID in [cidCall, cidAction]) then
  begin
    if (AQuery.FindColumnByAlias('base_DatFin') = nil) then
      AQuery.AddColumn(AQuery.BaseTable, 'DatFin', 'base_DatFin');

    if (AQuery.FindColumnByAlias('base_DatFup') = nil) then
      AQuery.AddColumn(AQuery.BaseTable, 'DatFup', 'base_DatFup');

    if (AQuery.FindColumnByAlias('base_DatIns') = nil) then
      AQuery.AddColumn(AQuery.BaseTable, 'DatIns', 'base_DatIns');
  end;
end;

{
  Uses the attributes of the current business class to create the columns.

  @param AGrid TcxGrid grid we are working on.
  @param ALevel TcxGridLevel object the view is added to.
  @param ADetailView TcxGridTableView destination view for the new columns
  @param AQuery select query for adding the columns to be selected
  @param AClass TBusinessObject containing the attributes we're currently adding
  @param ARefClass TBusinessObject class we started with when adding the columns.
  @param aViewNm string containing the name of the view. Can't this come from
         the view ??
  @param bAddSubLevels indicates if we will be adding the BOList attributes as
         SubLevels for the current level.

  @param ABOAttr Associated attribute of the parents
  @param AAttrPath AttributePath containing thepath for columns from the current
         class.
}
function ClassToView(
  AGrid: TcxGrid;
  ALevel: TcxGridLevel;
  ADetailView: TcxGridDBTableView;
  AQuery: TSelectQuery;
  AClass,
  ARefClass,
  AMasterViewClass: TBusinessObject;
  AViewNm: string;
  AAddSublevels: boolean;
  ABOAttr: TSingleBOAttribute = nil;
  AAttrPath: TAttributePath = nil;
  AAddDisplayFld: Boolean = false;
  ATable: TTable = nil): Integer;
var
  Idx: integer;
  oColumn: TcxGridDBColumn;

  QTable: TTable;
  QColumn: TColumn; // for sql query
  oStatusCol: TcxGridDBColumn;
  oView: TcxGridDBTableView;
  bSkipColumn: boolean;
  Attr: TAttribute;

  AttrPath: TAttributePath;
  IsCircularRef,
  IsDisallowedColumn,
  IsUsedDisplayAttr : Boolean;

  TreeTbl: TTable;
  FuncParams: string;
  sStartDLT,
  sEndDLT,
  sSoD,
  sEoD: string;
begin
  Result := 0;

  // What is the table we will be working on within this scope
  if ATable = nil then
    QTable := AQuery.BaseTable
  else
  begin
    // Create an alias unique for this scope.
    Assert(ARefClass <> nil);
    QTable := TTable.Create(AClass.TblName,
      ATable.Alias + '_' + ABOAttr.FldName + IntToStr(ABOAttr.ID));
  end;

  // loop over the attributes of the class
  if AAttrPath = nil then
    AttrPath := TAttributePath.Create(AClass)
  else
    AttrPath := TAttributePath.Create(AAttrPath);

  // Adding attributes to the current gridview.
  for idx := 0 to AClass.AttributeList.Count-1 do
  begin
    Attr := AClass.AttributeList[Idx];

    // Add the fieldname to the AttributePath
    Assert(Attr.FldName <> '');
    AttrPath.Add(Attr.FldName);

    // (1)
    // Skip column when the attribute is a singleBOAttribute and it's BO refers
    // back to the base class used in the master grid view.
    IsCircularRef :=
      ((Attr is TSingleBOAttribute)
      and (AMasterViewClass <> nil)
      and (AMasterViewClass.ClassID =
            TSingleBOAttribute(Attr).GetClass.ClassID));
    // (2)
    // Skip the column when the attribute is not in the list of attributes
    // allowed to be added to this grid view.
    IsDisallowedColumn := ((AttrPath = nil) or
      (AttrPath.BaseClass.GridAttributes.IndexOf(AttrPath.Value) = -1));
    // (3)
    // unless the attribute is the display attribute of the ABOAttr param.
    IsUsedDisplayAttr := ((ABOAttr <> nil) and (Attr.FldName = ABOAttr.DisplayFld)
        and AAddDisplayFld);

    bSkipColumn := IsCircularRef
      or (IsDisallowedColumn and not IsUsedDisplayAttr)
      or ((Attr is TSingleBOAttribute)
        and not ((Attr is TCallNumberAttribute) and (AClass.ClassID = cidCall)));
        
    if not ((Attr.GetMetaType in [mtBOList, mtUnknown]) or bSkipColumn) then
    begin
      Inc(Result);
      // When a status column is detected we need to add two columns instead
      // of one one for the checkbox and the other for the status text to
      // to be displayed.
      oColumn := aDetailView.CreateColumn as TcxGridDBColumn;
      oColumn.Visible := false;
      with oColumn do
      begin
        if AAttrPath = nil then
          Caption := Attr.Name
        else if IsUsedDisplayAttr then
          Caption := AAttrPath.AttrText
        else
          Caption := AttrPath.AttrText;

        Name := aViewNm + '_' + Attr.FldName;
        {$IFDEF DEBUG}
        Log.Fmt('Adding gridcolumn Index: %d Caption: ''%s'' Name:''%s''', [Index, Caption, Name], DEBUG_LOG);
        {$ENDIF}
        Options.Editing := False;
        case Attr.GetMetaType of
          mtID:
            begin
              DataBinding.ValueTypeClass := TcxIntegerValueType;
              Visible := False;
            end;
          mtCallNumber,
          mtInteger:
            DataBinding.ValueTypeClass := TcxIntegerValueType;
          mtCurrency:
            begin
              DataBinding.ValueTypeClass := TcxCurrencyValueType;
              PropertiesClass := TcxCurrencyEditProperties;
            end;
          mtHyperlink,
          mtString,
          mtStringCombo,
          mtStringSelect:
            DataBinding.ValueTypeClass := TcxStringValueType;
          mtDateTime,
          mtDate,
          mtTime:
            begin
              DataBinding.ValueTypeClass := TcxDateTimeValueType;
            end;
          mtTreeCombo:DataBinding.ValueTypeClass := TcxStringValueType;
          mtMemo:
            begin
              // Show the text instead of the text as memotype
              DataBinding.ValueTypeClass := TcxStringValueType;
              Options.Sorting := False;
              // First memo field will be the preview column
              { TODO : Use as preview only when selected by the user for field
                specific. }
{              if (aDetailView.Preview.Column = nil) then
                IsPreview := True;}
            end;
          mtState:
            begin
              if (ABOAttr = nil) then
              begin
                // this is the databinding for the first column
                DataBinding.ValueTypeClass := TcxBooleanValueType;
                PropertiesClass := TcxCheckBoxProperties;
                Name := aViewNm + '_StateCheckBox';
                DataBinding.FieldName := 'StateCheckBox';
                OnCustomDrawCell := TCustomColumnHandler.Create.DoCustomDrawCell;
                GetProperties.ImmediatePost := True;
                Index := 0;
                Width := 20;
                Options.Editing := True;

                // add the second column.
                oStatusCol := (aDetailView.CreateColumn as TcxGridDBColumn);
                oStatusCol.Name := aViewNm + '_' + Attr.Name + '2';
                oStatusCol.Caption := Attr.Name;
                oStatusCol.Options.Editing := False;
                oStatusCol.DataBinding.FieldName := 'State';
              end
            end;
          else
            DataBinding.ValueTypeClass := TcxVariantValueType;
        end; // Case

        // If not a calculated field set the databinding
        if (Attr.MetaType <> mtState) then
        begin
          { TODO : The joins for the mtTreeCombo have to be reviewed and posibly changed }
          if (Attr.MetaType = mtTreeCombo) then
          begin
            TreeTbl := TTable.Create('SCMTrees', Attr.FldName);
            AQuery.AddJoin(QTable, TreeTbl, Attr.FldName, 'ID', LEFT_JOIN);
            QColumn := AQuery.AddColumn(TreeTbl, 'Description', QTable.Alias + '_' + Attr.FldName)
          end
          else
            QColumn := AQuery.AddColumn(QTable,
                                        Attr.FldName,
                                        QTable.Alias + '_' + Attr.FldName);
          DataBinding.FieldName := QColumn.Alias;
        end;
      end;
    end
    else
    if AAddSubLevels and (Attr.GetMetaType = mtBOList) then
    begin
      // Create a sub view.
      oView := CreateView(
          AGrid,
          ALevel,
          BOMgr.BOList.GetByID(TBOListAttribute(Attr).ClassID),
          AClass,
          TBOListAttribute(Attr),
          AViewNm + IntToStr(TBOListAttribute(Attr).ClassID),
          False{AAddSublevels});
      oView.OptionsView.GroupByBox := False;
      oView.OptionsData.Deleting := False;
    end;
    // Attribute a single BO attribute then it should be traversed to add the
    //
    if (Attr is TSingleBOAttribute) then
    begin

      // Columns of associated classes can be added only when:
      // we are not currently adding columns of an associated class not too deep
      // and the current attribute is not a call number or is a call
      // number but not for the call class.
      if {1} ((AttrPath.Count < 4)) and
         {2} ((Attr.GetMetaType <> mtCallNumber) or
         {3}  ((Attr.GetMetaType = mtCallNumber)
                and (AClass.ClassID <> cidCall))) then
      begin
        // Add the columns for this association attribute to the
        // current level.
        // @note DO not add sublevels for this class.
        Result := Result + ClassToView(AGrid, ALevel, ADetailView, AQuery,
          TSingleBOAttribute(Attr).GetClass, AClass, AMasterViewClass,
          aViewNm + '_' + Attr.FldName, false, TSingleBOAttribute(Attr),
          AttrPath, not IsDisallowedColumn, QTable);
      end;
    end;
    AttrPath.RemoveTail(1);
  end;
  if (AClass.ClassID = cidCall) and (ATable = nil) then
  begin
    oColumn := aDetailView.CreateColumn as TcxGridDBColumn;
    with oColumn do
    begin
      Visible := False;
      Caption := 'Doorlooptijd';
      Name := aViewNm + '_DLT';
      DataBinding.ValueTypeClass := TcxVariantValueType;
      DataBinding.FieldName := 'DLT';
      Options.Editing := False;
      // Show this field as a MaskEdit to make sure that there are only 2 digits as fraction
      PropertiesClass := TcxMaskEditProperties;
      with TcxMaskEditProperties(Properties) do
      begin
        MaskKind := emkRegExprEx;
        EditMask := '[\d]+.\d{2}';
      end;
    end;
    sStartDLT := 'DatIns';
    sEndDLT := 'DatFin';
    sSoD := '510';
    sEoD := '1020';
    AQuery.AddFunctionColumn(QTable,
      'dbo.fn_SCMDuration('+QTable.Alias+'.'+sStartDLT+', '
      +QTable.Alias+'.'+sEndDLT+', GetDate(), '+QTable.Alias+'.Call_ID,'
      +QTable.Alias+'.Call_OnHold, '+sSoD+', '+sEoD+')',
      'DLT');
    inc(Result);
    AQuery.AddColumn(QTable, 'Call_OnHold', 'Call_OnHold');
  end;
  if (AClass.ClassID in [cidAction, cidCall]) and (ATable = nil) then
  begin
{    // Test for calculating state in query
    oColumn := aDetailView.CreateColumn as TcxGridDBColumn;
    with oColumn do
    begin
      Visible := True;
      Caption := 'DB_Status';
      Name := aViewNm + '_DB_State';
      DataBinding.ValueTypeClass := TcxVariantValueType;
      DataBinding.FieldName := 'DBState';
      Options.Editing := False;
    end;
    if (AClass.ClassID = cidCall) then
      FuncParams := QTable.Alias+'.DatFup, '+QTable.Alias+'.DatFin, '
                    +QTable.Alias+'.Call_OnHold, GetDate(), '
                    +QTable.Alias+'.Call_ID, 3'
    else
      FuncParams := QTable.Alias+'.DatFup, '+QTable.Alias+'.DatFin, NULL, '
                    +'GetDate(), '+QTable.Alias+'.Act_CallID, 3';

    AQuery.AddFunctionColumn(QTable,
      'dbo.fn_SCMStatus('+FuncParams+')',
      'DBState');
    inc(Result);
}
  end;

  if (Result > 0) and (ABOAttr <> nil) then
  begin
    AQuery.AddJoin(ATable, QTable, ABOAttr.FldName,
      AClass.GetIDAttribute.FldName, LEFT_JOIN);
    AQuery.Joins.Move(AQuery.Joins.Count-1, 0);
  end;

  if (ARefClass = nil) then
    AddRequiredColumns(AClass, AQuery);

  FreeAndNil(AttrPath);
end;

function GetValidIdent(AName: string): string;
var nm: string;
    i: integer;
begin
  nm := '';
  for i := 1 to Length(AName) do
  begin
    nm := nm + AName[i];
    if not IsValidIdent(nm) then
      nm[i] := '_';
  end;
  Result := nm;
end;

{
  Generate the columns in a grid from the model and data of the business class.

  @param aLevel A gridlevel object
  @param aGrid The Grid We are working on
  @param aDS The dataset containing the actual data of the business objects for
         the current class;
  @param classID The classID of the objects currently in view ??
  @param aViewNm The name used for this view.

  @return The new detailview on this gridlevel for the current grid row.
}
function CreateView(aGrid: TcxGrid; aLevel: TcxGridLevel;
    oClass, oMasterViewClass: TBusinessObject; oAttr: TBOListAttribute;
    aViewNm: string{; stSQL: string}; bAddSublevels: boolean; bIsSearch: boolean;
    oCtrl: TControl): TcxGridDBTableView;
var
  aDetailView: TcxGridDBTableView;
  Query: TSelectQuery;
begin
  {$IFDEF debug}Log.Add('uBOListDataSource - CreateView (using dataset) - ' + aViewNm); {$ENDIF}
  with aLevel do
  begin
    // Create a new view.
    aDetailView := aGrid.CreateView(TcxGridDBTableView) as TcxGridDBTableView;
    aDetailView.Tag := oClass.ClassID;
    // connect to level.
    ConnectViewToLevel(aGrid, aLevel, aDetailView);

    // Set view properties.
    if not IsValidIdent(aViewNm) then
      aViewNm := GetValidIdent(aViewNm);

    aDetailView.Name := aViewNm;


    ConnectGridEvents(aGrid, aDetailView, oClass.ClassID);

    // Allow the view to be searched
    aDetailView.DataController.Options := [dcoAnsiSort];
    aDetailView.OptionsData.Editing := True;
    with aDetailView.OptionsBehavior do
    begin
      ImmediateEditor := True;
      IncSearch := True;
    end;

    // Set the behaviour of the View
    aDetailView.OptionsSelection.HideFocusRect := False;
    aDetailView.OptionsSelection.MultiSelect := True;
    aDetailView.OptionsData.Deleting := False;
    aDetailView.ClearItems;

    // Add the columns to the view and create the Select statement at the same
    // time.
    Query := TSelectQuery.Create(TTable.Create(oClass.TblName, 'base'));

    try
      // Add columns to the view
      ClassToView(aGrid, aLevel, aDetailView, Query, oClass, nil,
        oMasterViewClass, aViewNm, oMasterViewClass = nil);

      if oAttr <> nil then
      begin
        Query.AddColumn(Query.BaseTable, oAttr.FldName,
          'base_' + oAttr.FldName);
        Query.AddJoin(
          TTable.Create(oMasterViewClass.TblName),
          oAttr.FldName,
          oMasterViewClass.GetIDAttribute.FldName, LEFT_JOIN);
//        Query.AddOrder(Query.BaseTable,
//          oClass.GetIDAttribute.FldName, ASCENDING);
      end;

      // Set the grid's properties according to the class.
      SetViewProperties(aDetailView, oClass, oMasterViewClass, oAttr, Query);
    finally
      FreeAndNil(Query);
    end;
  end;

  // Lastly read the layout of the grid view and return the grid.
  Result := aDetailView;

  {$IFDEF debug}Log.Add('uBOListDataSource - CreateView (end)'
    + ' - '+aViewNm, DEBUG_LOG);{$ENDIF}
end;

{
  FillGrid using a dataset.
}
function FillGrid(aClassID: integer; aGrid: TcxGrid; aViewNm: string;
  stSQL: string; bAddSublevels: boolean; bIsSearch: boolean;
  oCtrl: TControl): TcxCustomGridView; overload;
var tmpNm: string;
begin
  {$IFDEF debug}Log.Add('uBOListDataSource - FillGrid - Start'); {$ENDIF}
  aGrid.BeginUpdate;

  tmpNm := StringReplace(aViewNm, ' ', '_', [rfReplaceAll]);

  Result := CreateView(aGrid, aGrid.Levels, BOMgr.BOList.GetByID(aClassID), nil, nil,
    tmpNm, bAddSublevels, bIsSearch, oCtrl);

  aGrid.EndUpdate;
  {$IFDEF debug}Log.Add('uBOListDataSource - FillGrid - End'); {$ENDIF}
end;

function GetValueBufferFromAttribute(anAttribute: TAttribute): PChar;
begin
  if anAttribute.IsNull then
    Result := nil
  else
    case anAttribute.GetMetaType of
      mtID:         Result := @(TIDAttribute(anAttribute).fValue);
      mtInteger:    Result := @(TIntegerAttribute(anAttribute).fValue);
      mtIntCalc:    Result := @(TIntCalcAttribute(anAttribute).fValue);
      mtDouble:     Result := @(TDoubleAttribute(anAttribute).fValue);
      mtCurrency:   Result := @(TCurrencyAttribute(anAttribute).fValue);
      mtString:     Result := @(TStringAttribute(anAttribute).fValue);
      mtStringCombo:Result := @(TStringComboAttribute(anAttribute).fValue);
      mtStringSelect:Result := @(TStringSelectAttribute(anAttribute).fValue);
      mtDateTime:   Result := @(TDateTimeAttribute(anAttribute).fValue);
      mtDate:       Result := @(TDateAttribute(anAttribute).fValue);
      mtTime:       Result := @(TTimeAttribute(anAttribute).fValue);
      mtMemo:       Result := @(TMemoAttribute(anAttribute).fValue);
      mtHyperLink:  Result := @(THyperLinkAttribute(anAttribute).fValue);
      mtCallNumber,
      mtBOCombo,
      mtBOSearch,
      mtBOTree:     Result := TSingleBOAttribute(anAttribute).ValuePtr;
      mtState:      Result := @(TStateAttribute(anAttribute).IsFinished);
    end;
end;

procedure TCustomColumnHandler.DoCustomDrawCell(
  Sender: TcxCustomGridTableView; ACanvas: TcxCanvas;
  AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
var
  Item : TcxCustomGridTableItem;
begin
  if AViewInfo.EditViewInfo is TcxCustomCheckBoxViewInfo then
  begin
    Item := TcxGridDBTableView(Sender).DataController
      .GetItemByFieldName('base_DatFin');
    if Item <> nil then
    begin
      TcxCustomCheckBoxViewInfo(AViewInfo.EditViewInfo).State :=
          TcxCheckBoxState(AViewInfo.GridRecord.Values[Item.Index] <> null);
    end;
  end;
end;

end.
