unit uGridBldr73;

interface

uses
  Classes,
  SysUtils,
  cxDataStorage,
  cxGrid,
  cxGridLevel,
  cxGridCustomView,
  cxGridTableView,

  uModel,
  uObjDataSource;

type
  TGridItemEvent = procedure(ASender: TObject; ALevel: TcxGridLevel;
    AItem: TObject) of object;

  TGridItemComposer = class;

  TGridItemInfo = class
  private
    FAttrPath: string;
    FAttrDef: TAttributeDef;
    procedure SetAttrDef(const Value: TAttributeDef);
    procedure SetAttrPath(const Value: string);

  public
    constructor Create;

    property AttrPath: string read FAttrPath write SetAttrPath;
    property AttrDef: TAttributeDef read FAttrDef write SetAttrDef;
  end;

  TGridBuilder = class;

  (**
    Abstract class for creating an object within the grid at specified level
    for a given attribute definition.
  **)
  TGridItemComposer = class(TObject)
  private
    FBuilder: TGridBuilder;

  public
    constructor Create(ABuilder: TObject);

    procedure Compose(ALevel: TcxGridLevel; AAttrDef: TAttributeDef); virtual;
      abstract;

    function CanHandle(AAttr: TAttributeClass): Boolean; virtual; abstract;

    property Builder: TGridBuilder read FBuilder;
  end;

  (**
    Creates a column in the grid at the given level for the AttributeDef.
  **)
  TGridColumnComposer = class(TGridItemComposer)
  private
    FColumn : TcxGridColumn;
  public
    procedure Compose(ALevel: TcxGridLevel; AAttrDef: TAttributeDef); override;
    function CanHandle(AAttr: TAttributeClass): Boolean; override;
  end;

  (**
    Create a column in the grid at the given level specific for a status column
  **)
  TGridStatusColumnComposer = class(TGridColumnComposer)
  public
    procedure Compose(ALevel: TcxGridLevel; AAttrDef: TAttributeDef); override;
    function CanHandle(AAttr: TAttributeClass): Boolean; override;
  end;

  (**
    Creates a new view in the grid at the given level for the AttributeDef.
  **)
  TGridViewComposer = class(TGridItemComposer)
  private
    //* Returns a composer for the given class
    function GetComposer(AAttrClass: TAttributeClass): TGridItemComposer;

    //* Creates a top level view for the grid
    function CreateView(AGrid: TcxGrid; ABC: TBusinessClass;
      ACriteria: string = ''): TcxCustomGridView; overload;

    //* Creates a detail view for the grid.
    function CreateView(AGrid: TcxGrid; ADS: TObjectDataSource;
      AAttr: TAttributeDef): TcxCustomGridView; overload;

    //* Compose all attributes onto the given view and level.
    procedure ComposeAttributes(ALevel: TcxGridLevel; ABC: TBusinessClass);

  public
    //* Composes a new grid view for an attribute definition.
    procedure Compose(ALevel: TcxGridLevel; AAttrDef: TAttributeDef); overload;
      override;

    //* Composes a new grid view for a business class
    procedure Compose(AGrid: TcxGrid; ABC: TBusinessClass;
      ACriteria: string = ''); reintroduce; overload;

    //* Determines if this compooser can compose for the given attribute class.
    function CanHandle(AAttr: TAttributeClass): Boolean; override;

  end;

  (**

    Use the grid builder to create a grid containing attributes as columns
    and views.

    The builder uses a simple if statement to determine the composer to use
    for an attribute.

  **)
  TGridBuilder = class(TObject)
  private
    FComposers : TList;

    FViewComposer : TGridViewComposer;

    FOnView: TGridItemEvent;
    FOnViewItem: TGridItemEvent;

    //* Return the appropriate composer for the attribute class
    function GetComposer(AClass: TAttributeClass): TGridItemComposer;

  public
    //* Constructs the grid builder
    constructor Create;

    //* Destructs the grid builder
    destructor Destroy; override;

    //* Register a composer to the builder
    procedure Register(AComposer: TGridItemComposer);

    //* build a grid on the component showing the objects for a business class.
    function BuildGrid(AOwner: TComponent; AName: String; ABC: TBusinessClass;
      ACriteria: string = ''): TcxGrid;

    //* Frees a grid build with the gridbuilder.
    class procedure CleanseGrid(AGrid: TcxGrid);

    //* Returns the composer used to create a new view to the grid.
    property ViewComposer : TGridViewComposer read FViewComposer;

    //* event which fires after a grid view has been created.
    property OnView: TGridItemEvent read FOnView write FOnView;

    //* Event which fires after a grid item (e.g. column) has been created.
    property OnViewItem: TGridItemEvent read FOnViewItem write FOnViewItem;
  end;

const
  SCMGridDataBinding: array [mtID..mtUnknown] of TcxValueTypeClass = (
    TcxIntegerValueType, TcxIntegerValueType,  TcxIntegerValueType,  TcxIntegerValueType,
    TcxFloatValueType,   TcxCurrencyValueType, TcxStringValueType,   TcxStringValueType,
    TcxStringValueType,  TcxDateTimeValueType, TcxDateTimeValueType, TcxDateTimeValueType,
    TcxStringValueType,  TcxStringValueType,   TcxStringValueType,   TcxVariantValueType,
    TcxVariantValueType, TcxStringValueType,   TcxStringValueType,   TcxStringValueType,
    TcxVariantValueType, TcxVariantValueType,  TcxVariantValueType);

implementation

uses
  Contnrs,
  cxCustomData,
  cxGraphics,
  cxCheckbox,
  cxGridPopupMenu,
  uLogger,
  uSCMConst, StrUtils; // Needed when creating the state attribute.

{ TGridBuilder }

(**
  Creates a grid for use within SCM
**)
function TGridBuilder.BuildGrid(AOwner: TComponent; AName: String;
  ABC: TBusinessClass; ACriteria: string): TcxGrid;
var
  Grid: TcxGrid;
begin
  Grid := TcxGrid.Create(AOwner);
  Grid.Name := AName;
  ViewComposer.Compose(Grid, ABC, ACriteria);
  (TcxGridPopupMenu.Create(AOwner) as TcxGridPopupMenu).Grid := Grid;
  Result := Grid;
end;

procedure TGridBuilder.Register(AComposer: TGridItemComposer);
begin
  FComposers.Add(AComposer);
end;

destructor TGridBuilder.Destroy;
begin
  FreeAndNil(FComposers);
  inherited;
end;

{ TGridViewComposer }

function TGridViewComposer.CanHandle(AAttr: TAttributeClass): Boolean;
begin
  result := AAttr.InheritsFrom(TBOListAttribute);
end;

(**
  Creates a detail level gridview.
**)
procedure TGridViewComposer.Compose(ALevel: TcxGridLevel;
  AAttrDef: TAttributeDef);
var
  Grid : TcxGrid;
  Level : TcxGridLevel;
  DS : TObjectDataSource;
  ItemInfo: TGridItemInfo;
begin
  // Only two levels including top level allowed
  if not (ALevel.IsTop) then Exit;

  // Only one sub level allowed on a level
  if ALevel.Count > 0 then Exit;

  // Grid is reffered by ALevel.Control
  Grid := TcxGrid(ALevel.Control);

  // Create a new level and view under the given level.
  Level := ALevel.Add;
  Level.Name := LeftStr(ALevel.Name, Length(ALevel.Name) - 4) + '_'
    + AAttrDef.FldName + '_lvl';
  DS := TObjectDataSource(ALevel.GridView.Datacontroller.CustomDataSource);
  Level.GridView := CreateView(Grid, DS, AAttrDef);
  Level.GridView.Name := LeftStr(Level.Name, Length(Level.Name)-4) + '_view';

  // Add the info object to the view
  ItemInfo := TGridItemInfo.Create;
  ItemInfo.AttrPath := TGridItemInfo(Level.Parent.GridView.Tag).AttrPath
                        + '.' + AAttrDef.FldName;
  ItemInfo.AttrDef := AAttrDef;
  Level.GridView.Tag := integer(ItemInfo);

  Log.Fmt('New gridview Name=''%s''', [Level.GridView.Name]);

  // Show the tabs if needed.
  if ALevel.Count <= 1 then
    ALevel.Options.DetailTabsPosition := dtpNone
  else
    ALevel.Options.DetailTabsPosition := dtpTop;  // Default tabs position.

  Level.Caption := AAttrDef.Name;

  // adding columns
  ComposeAttributes(Level, AAttrDef.BOClass);

  if Assigned(Builder.OnView) then Builder.OnView(Self, Level, Level.GridView);
end;

(**
  Creates a top level grid view containing columns for the business class.

  @param AGrid the grid on which the view is composed
  @param ABC the business class for which the grid view is being build.
  @param ACriteria
**)
procedure TGridViewComposer.Compose(AGrid: TcxGrid; ABC: TBusinessClass;
  ACriteria: string);
var
  Level: TcxGridLevel;
  ItemInfo: TGridItemInfo;
  BaseName: String;
begin
  Level := AGrid.Levels.Add;

  // use level name if it is available. Added because renaming the grid doesn't work.
  BaseName := AGrid.Levels.Name;
  if BaseName = '' then BaseName := AGrid.Name;

  Level.Name := BaseName + '_lvl';
  Level.GridView := CreateView(AGrid, ABC, ACriteria);
  Level.GridView.Name := LeftStr(Level.Name, Length(Level.Name) - 4) + '_view';

  // Add the info object to the view
  ItemInfo := TGridItemInfo.Create;
  ItemInfo.AttrPath := '#' + IntToStr(ABC.ID);
  Level.GridView.Tag := Integer(ItemInfo);

  Level.Options.DetailTabsPosition := dtpTop;

  ComposeAttributes(Level, ABC);

  if Assigned(Builder.OnView) then Builder.OnView(Self, Level, Level.GridView);
end;

(**
  Creates a new view and connects the datacontroller to it.
**)
procedure TGridViewComposer.ComposeAttributes(ALevel: TcxGridLevel;
  ABC: TBusinessClass);
var
  I : Integer;
  Composer : TGridItemComposer;
begin
  // Walk each attribute and add it to the current level
  for I := 0 to ABC.Attributes.Count-1 do
  begin
    Composer := GetComposer(ABC.Attributes[I].InstanceClass);
    if (Composer <> nil) then
      Composer.Compose(ALevel, ABC.Attributes[I]);
  end;
end;

(**
  Creates a new top level view on the grid.
  Set default options for the view
**)
function TGridViewComposer.CreateView(AGrid: TcxGrid; ABC: TBusinessClass;
  ACriteria: string): TcxCustomGridView;
var
  DS : TObjectDataSource;
begin
  Result := AGrid.CreateView(TcxGridTableView);
  with Result as TcxGridTableView do
  begin
    DS := TObjectDataSource.Create(ABC, ACriteria);
    DataController.CustomDataSource := DS;

    // Let the CustomDataSource know when the events are fired
    DataController.OnDetailCollapsing := DS.HandleDetailCollapsing;
    DataController.OnDetailExpanding := DS.HandleDetailExpanding;

    OnColumnHeaderClick := DS.HandleColumnHeaderClick;

    with OptionsData do
    begin
      Editing := True;
      Deleting := False;
    end;
    OptionsView.GroupByBox := False;
    with OptionsBehavior do
    begin
      ImmediateEditor := True;
      IncSearch := True;
    end;
  end;
end;

(**
  Create a new detail level view on the grid.
**)
function TGridViewComposer.CreateView(AGrid: TcxGrid; ADS: TObjectDataSource;
  AAttr: TAttributeDef): TcxCustomGridView;
var
  DS: TObjectDataSource;
begin
  Result := AGrid.CreateView(TcxGridTableView);
  with Result as TcxGridTableView do
  begin
    DS := TDetailObjectDataSource.Create(ADS, AAttr);
    DataController.CustomDataSource := DS;

    // Let the CustomDataSource know when the events are fired
    DataController.OnDetailCollapsing := DS.HandleDetailCollapsing;
    DataController.OnDetailExpanding := DS.HandleDetailExpanding;

    // Default properties
    OptionsView.GroupByBox := False;
    OptionsView.Footer := False;
  end;
end;

function TGridViewComposer.GetComposer(
  AAttrClass: TAttributeClass): TGridItemComposer;
begin
  Result := FBuilder.GetComposer(AAttrClass);
end;

{ TGridColumnComposer }

function TGridColumnComposer.CanHandle(AAttr: TAttributeClass): Boolean;
begin
  result := AAttr.InheritsFrom(TValueAttribute) or
    AAttr.InheritsFrom(TBOAttribute);
end;

procedure TGridColumnComposer.Compose(ALevel: TcxGridLevel;
  AAttrDef: TAttributeDef);
var ItemInfo: TGridItemInfo;
begin
  Assert(ALevel.GridView <> nil,
    'Must have a view attached before adding columns.');

  Assert(ALevel.GridView is TcxGridTableView,
    'This composer is only useful for a TcxGridTableView');

  Assert(AAttrDef.InstanceClass <> TBOListAttribute, 'Invalid attribute');

  FColumn := TcxGridTableView(Alevel.GridView).CreateColumn as TcxGridColumn;
  with FColumn do
  begin
    Caption := AAttrDef.Name;
    DataBinding.ValueTypeClass := SCMGridDataBinding[AAttrDef.MetaType];
    DataBinding.Data := Pointer(AAttrDef.Index);
    Options.Editing := False;

    // Add the GridItemInfo
    ItemInfo := TGridItemInfo.Create;
    ItemInfo.AttrPath := TGridItemInfo(ALevel.GridView.Tag).AttrPath
                          + '.' + AAttrDef.FldName;
    ItemInfo.AttrDef := AAttrDef;
    Tag := integer(ItemInfo);
  end;

  if Assigned(Builder.OnViewItem) then Builder.OnViewItem(Self, ALevel, FColumn);
end;

constructor TGridBuilder.Create;
begin
  // Create the composer list
  FComposers := TObjectList.Create;

  FViewComposer := TGridViewComposer.Create(Self);

  // Register the composers this builder uses
  Register(TGridStatusColumnComposer.Create(Self)); // BEFORE TGridColumnComposer
  Register(TGridColumnComposer.Create(Self));
  Register(FViewComposer);
end;

(**
  Return the composer which is capable of handling the given AttributeClass
**)
function TGridBuilder.GetComposer(
  AClass: TAttributeClass): TGridItemComposer;
var
  I : Integer;
begin
  if (AClass <> TUnknownAttribute) then
    for I := 0 to FComposers.Count-1 do
    begin
      Result := FComposers[I];
      if Result.CanHandle(AClass) then Exit;
    end;
  Result := nil;
end;

{ TGridItemComposer }

constructor TGridItemComposer.Create(ABuilder: TObject);
begin
  FBuilder := ABuilder as TGridBuilder;
end;

{ TGridStatusColumnComposer }

function TGridStatusColumnComposer.CanHandle(
  AAttr: TAttributeClass): Boolean;
begin
  Result := AAttr.InheritsFrom(TStateAttribute);
end;

(**
  Will create the status column onto the grid.
**)
procedure TGridStatusColumnComposer.Compose(ALevel: TcxGridLevel;
  AAttrDef: TAttributeDef);
var Column : TcxGridColumn;
    ItemInfo: TGridItemInfo;
begin
  //* First create the normal column.
  inherited;

  // Change the binding of the column (It should use the displayvalue
  FColumn.DataBinding.Data := Pointer(not AAttrDef.Index);

  // add the checkbox column
  Column := TcxGridTableView(Alevel.GridView).CreateColumn as TcxGridColumn;
  with Column do
  begin
    Caption := AAttrDef.Name;
    PropertiesClass := TcxCheckBoxProperties;
    DataBinding.ValueTypeClass := TcxBooleanValueType;
    DataBinding.Data := Pointer(AAttrDef.Index);

    // Add the GridItemInfo
    ItemInfo := TGridItemInfo.Create;
    ItemInfo.AttrPath := TGridItemInfo(ALevel.GridView.Tag).AttrPath
                          + '.' + AAttrDef.FldName;
    ItemInfo.AttrDef := AAttrDef;
    Tag := integer(ItemInfo);

//    OnCustomDrawCell := TCustomColumnHandler.Create.DoCustomDrawCell;
    Properties.ImmediatePost := True;

    with Properties as TcxCheckBoxProperties do
    begin
      AllowGrayed := False;

      if AAttrDef.Owner.ID = cidCall then
        ValueChecked := stIsClosed
      else
        ValueChecked := stActionIsClosed;

      NullStyle := nssUnchecked;
    end;

    Index := 0;
    Width := 20;

    Options.Editing := True;
  end;
end;

{ TGridItemInfo }

constructor TGridItemInfo.Create;
begin
  FAttrDef := nil;
end;

procedure TGridItemInfo.SetAttrDef(const Value: TAttributeDef);
begin
  FAttrDef := Value;
end;

procedure TGridItemInfo.SetAttrPath(const Value: string);
begin
  FAttrPath := Value;
end;

(**
  Cleanses the grid, removing the TGridItemInfo objects attached to the views
  and columns in the grid. Also frees the custom data controllers atached to the
  used views.

  @param AGrid The grid to be cleansed.
**)
class procedure TGridBuilder.CleanseGrid(AGrid: TcxGrid);
var
  I, J : Integer;
begin
  if AGrid <> nil then
  begin
    // Clean up our own datacontrollers. (These seem to leak)
    for I := 0 to AGrid.ViewCount - 1 do
      if AGrid.Views[I] is TcxGridTableView then
      begin
        with TcxGridTableView(AGrid.Views[I]) do
        begin
          // Remove the TGridItemInfo items attached to the colums
          for J := 0 to ColumnCount -1 do
          begin
            if TObject(Columns[J].Tag) is TGridItemInfo then
            begin
              TGridItemInfo(Columns[J].Tag).Free;
              Columns[J].Tag := 0;
            end;
          end;

          // Free the TGridItemInfo attached to the view.
          if TObject(Tag) is TGridItemInfo then
          begin
            TGridItemInfo(Tag).Free;
            Tag := 0;
          end;

          // Free the custom data controller.
          with DataController do
            if CustomDataSource is TObjectDataSource then
            begin
              CustomDataSource.Free;
              CustomDataSource := nil;
            end;
        end;
      end;
  end;
end;

end.

