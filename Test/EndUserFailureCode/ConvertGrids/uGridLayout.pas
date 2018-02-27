unit uGridLayout;

interface

uses
  Classes, DB, cxGridLevel;

type
  {*
    Contains the layout for a grid.
  }
  TGridLayout = class (TObject)
  private
    FID: Integer;
    FClassID: Variant; // allow null
    FName: String;
    FIsDefault: Boolean;
    FDepId: Variant; // allow null
    FEmpID: Variant; // allow null
    FData: String;

    procedure GetIsDefault(const Value: Boolean);
    function GetName: String;
  public
    // Create
    constructor Create; overload;
    constructor Create(AID: Integer); overload;

    procedure Load;
    procedure LoadFrom(ADS: TDataSet);
    procedure Save;
    function SaveTo(ADS: TDataSet): Boolean;

    property ID: Integer read FID write FID;
    property ClassID: Variant read FClassID write FClassID;
    property Name: String read GetName write FName;
    property IsDefault: Boolean read FIsDefault write GetIsDefault;
    property Data: String read FData write FData;
  end;

  (**
    Contains a list of all grid layouts available in the system.
  **)
  TGridLayouts = class (TObject)
  private
    // List of TGridLayout objects.
    FList : TList;

    function GetItem(Index: Integer): TGridLayout;
    function GetCount: Integer;

  public
    // Constructors
    constructor Create;

    // Destructor
    destructor Destroy; override;

    // Load the available layouts in the system.
    procedure Load;

    // Load the layouts from a dataset.
    procedure LoadFrom(ADS: TDataSet);

    // Reloads the list of Layouts.
    procedure Reload;

    // Clears the list containing the layouts.
    procedure Clear;

    // Select the layouts for specified class id.
    function SelectFor(AClassID: Integer): TList;

    // Gets the layout for the given name.
    function Get(AClassID: Integer; AName: String = ''): TGridLayout;

    // Get the default for specified class.
    function GetDefault(AClassID: Integer): TGridLayout;

    // Returns a gridlayout by its name.
    function GetByName(AClassID: Integer; AName: string): TGridLayout;

    // Save the layouts back into the database
    procedure Save;

    // Adds and returns a grid layout in the internal list of layouts.
    function Add: TGridLayout;

    // Remove the filter information from the layouts
    procedure RemoveFiltersFor(AClassID : Integer);

    property Items[Index: Integer] : TGridLayout read GetItem; default;
    property Count: Integer read GetCount;
  end;

// Restore the layout to the user file.
procedure RestoreGridLayout(ALevel: TcxGridLevel);

// Store the layout to the user file.
procedure StoreGridLayout(ALevel: TcxGridLevel);

implementation

uses
  Forms, Dialogs, SysUtils, StrUtils, Variants, Contnrs, uDBMgr, ADODB,
  uDataMod, cxGridDBTableView, uTranslation, uCore, cxGridCustomView, uLogger,
  uSCMConst, cxGridTableView;

procedure RestoreGridLayout(ALevel: TcxGridLevel);
var
  GV : TcxCustomGridView;
  Level : TcxGridLevel;
  I, j : Integer;
begin
  Log.Fmt('Restoring the grid layout for ''%s''', [ALevel.Name], FORM_LOG);
  for i := 0 to (ALevel.Count - 1) do
  begin
    Level := ALevel[i];
    GV := Level.GridView;
    GV.RestoreFromIniFile(SCMGridFile, True, False,
      [gsoUseFilter, gsoUseSummary], GV.Name);

    with TcxGridTableView(Level.GridView) do
      for j := pred(ColumnCount) downto 0 do
        if (Columns[j].Caption = '') then
          Columns[j].Free;

    RestoreGridLayout(Level);
  end;
end;

procedure StoreGridLayout(ALevel: TcxGridLevel);
var
  GV : TcxCustomGridView;
  Level : TcxGridLevel;
  I : Integer;
begin
  Log.Fmt('Storing the grid layout for ''%s''', [ALevel.Name]);
  for i := 0 to ALevel.Count - 1 do
  begin
    Level := ALevel[i];
    GV := Level.GridView;
    GV.StoreToIniFile(SCMGridFile, False,
      [gsoUseFilter, gsoUseSummary], GV.Name);
    StoreGridLayout(Level);
  end;
end;


{ TGridLayout }

(**
  Constructs a TGridLayout object.

  @param AID integer containing the id for the new TGridLayout object.

  @throws Exception when the layout does not exist.
**)
constructor TGridLayout.Create(AID: Integer);
begin
  FID := AID;
  FClassID := null;
  FIsDefault := False;
  FName := '';
  FDepID := null;
  FEmpId := null;
  FData := '';
end;

(**
  Creates a new TGridLayout instance.
**)
constructor TGridLayout.Create;
begin
  Self.Create(0);
end;

(**
  Determines if the current object is a default object.
**)
procedure TGridLayout.GetIsDefault(const Value: Boolean);
begin
  FIsDefault := Value or (FName = '');
end;

(**
  Returns the name of the layout.
**)
function TGridLayout.GetName: String;
begin
  if FName = '' then
    FName := SCMTranslate(157); // 'Default'
{
  if IsDefault then
    Result := '(' + FName + ')'
  else
}
    Result := FName;

end;

(**
  Loads a layout from the database.
**)
procedure TGridLayout.Load;
begin
  FData := DBMgr.GetGridView(Self.ID);
end;

(**
  Loads a Layout from the current record of a recordset.
**)
procedure TGridLayout.LoadFrom(ADS: TDataSet);
begin
  Assert((ADS <> nil) and not ADS.EOF);
  try
    with ADS do
    begin
      FID := FieldByName('ID').AsInteger;
      FClassID := FieldByName('ClassID').AsVariant;
      FName := FieldByName('Name').AsString;
      FIsDefault := FieldByName('IsDefault').AsBoolean;
      FDepID := FieldByName('Dep_ID').AsVariant;
      FEmpID := FieldByName('Emp_ID').AsVariant;
      FData := FieldByName('Data').AsString;
    end;
  except
    FID := -1; // Not loaded if things went messy.
  end;
end;

(**
  Save the object back to the storage facility.
**)
procedure TGridLayout.Save;
var
  Query : TADOQuery;
begin
  Query := TADOQuery.Create(Application);
  try
    Query.Connection := DataMod.ADOCon;
    Query.SQL.Text :=
      'SELECT * FROM SCMGridViews WHERE ID=' + IntToStr(FID);
    Query.Open;
    Self.SaveTo(Query);
    Query.Close;
  finally
    FreeAndNil(Query);
  end;
end;


(**
  Store the record to an open dataset.
**)
function TGridLayout.SaveTo(ADS: TDataSet): Boolean;
begin
  Result := true;
  try
    with ADS do
    begin
      Edit; // Edit calls insert when ADS is an empty dataset
      FieldByName('ClassID').Value := FClassID;
      FieldByName('Dep_ID').Value := FDepID;
      FieldByName('Emp_ID').Value := FEmpID;
      FieldByName('Name').Value := FName;
      FieldByName('IsDefault').Value := FIsDefault;
      FieldByName('Data').Value := FData;
      Post; // Updates the record or newly created record
      FID := FieldByName('ID').AsInteger;
    end;
  except
    Result := False;
  end;

end;

{ TGridLayouts }

(**
  Adds a layout to the internal list.

  The layout should be unique
**)
function TGridLayouts.Add: TGridLayout;
begin
  Result := TGridLayout.Create(-1);
  FList.Add(Result);
end;

procedure TGridLayouts.Clear;
begin
  FList.Clear;
end;

constructor TGridLayouts.Create;
begin
  FList := TObjectList.Create;
end;

destructor TGridLayouts.Destroy;
begin
  FList.Free;
  inherited;
end;

function TGridLayouts.Get(AClassID: Integer; AName: String): TGridLayout;
begin
  if AName = '' then
    Result := GetByName(AClassID, AName)
  else
    Result := GetDefault(ACLassID);
end;

function TGridLayouts.GetByName(AClassID: Integer; AName: string): TGridLayout;
var
  I: Integer;
  Layout : TGridLayout;
begin
  for I := 0 to FList.count-1 do
  begin
    Layout := FList[I];
    if (Layout.ClassID = AClassID) and (Layout.Name = AName) then
    begin
      Result := Layout;
      exit;
    end;
  end;
  Result := nil;
end;

function TGridLayouts.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGridLayouts.GetDefault(AClassID: Integer): TGridLayout;
var
  I: Integer;
  Layout : TGridLayout;
begin
  for I := 0 to FList.count-1 do
  begin
    Layout := FList[I];
    if (Layout.ClassId = AClassID) and Layout.IsDefault then
    begin
      Result := Layout;
      exit;
    end;
  end;
  Result := nil;
end;

function TGridLayouts.GetItem(Index: Integer): TGridLayout;
begin
  Result := FList[Index];
end;

procedure TGridLayouts.Load;
var
  Qry : TADOQuery;
  Layout : TGridLayout;
begin
  Qry := TADOQuery.Create(nil);
  try
    Qry.Connection := DataMod.ADOCon;
    Qry.SQL.Add('SELECT * FROM SCMGridViews '
      + 'WHERE IsDefault=1 OR (Emp_ID=' + IntToStr(SCMUserCode) + ')');
    Qry.Open;
    while not Qry.EOF do
    begin
      Layout := Self.Add;
      Layout.LoadFrom(Qry);
      Qry.Next;
    end;
    Qry.Close;
  finally
    Qry.Free;
  end;
end;

(**
  Load all layouts from the given dataset.
**)
procedure TGridLayouts.LoadFrom(ADS: TDataSet);
begin
  ADS.First;
  while not ADS.Eof do
  begin
    Add.LoadFrom(ADS);
    ADS.Next;
  end;
end;

(**
  Reloads the list of layouts.
**)
procedure TGridLayouts.Reload;
begin
  FList.Clear;
  Load;
end;

(**
  Remove the filters from the gridlayouts currently stored.

  @param AClassID the class id for which we want to remove the filters.
**)
procedure TGridLayouts.RemoveFiltersFor(AClassID: Integer);
var
  I : Integer;
  Stream : TStringStream;
  Layout : TGridLayout;
  GridView : TcxGridDBTableView;
  Component : TComponent;
  bSelfMade : Boolean;
  nType : Byte;
  nStrLen : Byte;
  GridName : String;
  nPos : Integer;
  UseExistingView : Boolean;
  nStep: Integer;
begin

  nStep := 0;
  for I := 0 to FList.Count-1 do
  begin
    Layout := TGridLayout(FList[I]);
    if Layout.ClassID = AClassID then
    begin
      Log.Fmt('Removing filterinfo from layout id#%d', [Layout.ID]);
      // Layout found, remove the filterinfo from the layout.
      try
        nStep := 1;
        // Get a component form named FrmMain otherwise create it
        bSelfMade := False;
        Component := Application.FindComponent('FrmMain');
        if Component = nil then
        begin
          nStep := 2;
          Component := TForm.Create(Application);
          Component.Name := 'FrmMain';
          bSelfMade := True;
        end;

        UseExistingView := False;
        GridView := nil;
        try
          nStep := 3;
          // Restore the layout to a temp gridview.
          Stream := TStringStream.Create(Layout.Data);
          try
            nStep := 4;
            // Get the name of the View from the stream.
            { TODO : Make a function for this thingy }
            Stream.Read(nType, 1);
            Stream.Read(nStrLen, 1);
            GridName := Stream.ReadString(nStrLen);
            nPos := Pos('.', GridName);
            if nPos > 0 then
              GridName := MidStr(GridName, nPos + 1, nStrLen);

            nStep := 5;
            // Do we have a component with this name ?
            GridView := TcxGridDBTableView(Component.FindComponent(GridName));
            UseExistingView := GridView <> nil;
            if GridView = nil then
            begin
              GridView := TcxGridDBTableView.Create(Component);
              GridView.Name := GridName;
            end;

            nStep := 6;
            // Reset the stream and restore.
            Stream.Position := 0;
            GridView.RestoreFromStream(Stream, True, False, []);
          finally
            FreeAndNil(Stream);
          end;
          nStep := 7;
          // Store the layout for the view back into the.
          Stream := TStringStream.Create('');
          try
            GridView.StoreToStream(Stream, []);
            Layout.Data := Stream.DataString;
          finally
            FreeAndNil(Stream);
          end;
          nStep := 8;
          // Write the string back to the object containing the layout.
        finally
          if Not UseExistingView then
            FreeAndNil(GridView);
          if bSelfMade then
            FreeAndNil(Component);
        end;
      except
        MessageDlg('Er is een fout opgetreden tijdens verwijderen van '
          + 'filter informatie voor grids. Code(' + IntToStr(nStep) + ')',
          mtError, [mbOK], 0);
      end;
      Layout.Save;
    end;
  end;
end;


(**
  Store all the layouts back to the database.
**)
procedure TGridLayouts.Save;
begin

end;

(**
  Return all layouts for the given class.
**)
function TGridLayouts.SelectFor(AClassID: Integer): TList;
var
  I : Integer;
  Layout : TGridLayout;
  HasDefault : Boolean;
begin

  HasDefault := False;
  Result := TList.Create;
  for I := 0 to FList.Count-1 do
  begin
    Layout := TGridLayout(FList[I]);
    if (Layout.ClassID = AClassID) and (Layout.IsDefault or
      (Layout.FEmpID = SCMUserCode)) then
    begin
      HasDefault := HasDefault or Layout.IsDefault;
      Result.Add(Layout);
    end;
  end;

  // Make sure there is ALWAYS a default in the returned list.
  if not HasDefault then
  begin
    // We need to add it to our internal list otherwise it will not be
    // freed when self is freed.
    Layout := TGridLayout.Create;
    Layout.ClassID := AClassID;
    Layout.Name := '(' + SCMTranslate(157) + ')';
    FList.Add(Layout);
    Result.Add(Layout);
  end;
end;

end.

