unit uSQL;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs;

const
  ASCENDING = True;
  DESCENDING = False;

  const EQUALS = '=';
  const GREATER = '>';
  const LESS = '<';
  const GREATEREQUAL = '>=';
  const LESSEQUAL = '<=';
  const LIKE = 'LIKE';
  const ISNULL = 'IS';
  const ISNOTNULL = 'IS NOT';

type
  TJoinType = (JOIN, INNER_JOIN, LEFT_JOIN, RIGHT_JOIN);
  TGroupByType = (gbtDefault, gbtAll);

  TColumnOpts = Set of (coName, coAlias);

  TTable = class;
  TOutput = class;
  TSelectQuery = class;
  TJoin = class;

  // Used by the TSelectQuery for identifying the tables used
  TRefTable = class
  private
    FTable : TTable;
    FRef : Integer;
  public
    constructor Create(ATable: TTable);

    procedure IncRef;

    property Table : TTable read FTable;
    property Ref : Integer read FRef;
  end;

  TOutputable = class (TObject)
  public
    function ToString: string; virtual; abstract;
    procedure Write(AOut: TOutput); overload; virtual; abstract; 
  end;

  {
    Output class
  }
  TOutput = class (TObject)
  private
    FCurrentIndent: string;
    FIndent: string;
    FLineBreak: string;
    FNewLineComing: Boolean;
    FResult: TStringStream;
    procedure WriteNewLineIfNeeded;
  public
    // Constructor of this class
    constructor Create(indent: String); overload;
    constructor Create(indent: String; LineBreak: string); overload;

    destructor Destroy; override;

    procedure indent;
    function ToString: string;
    procedure unindent;
    function Write(ch: Char): TOutput; overload;
    function Write(s: string): TOutput; overload;
    function Write(obj: TOutputable): TOutput; overload; stdcall;
    function WriteLn: TOutput; overload;
    function WriteLn(s: string): TOutput; overload;
    function WriteLn(obj: TOutputable): TOutput; overload;
  end;

  TColumn = class (TOutputable)
  private
    FTable: TTable;
    FIsFunction: Boolean;
    function GetTable: TTable;

  protected
    FName: string;
    FAlias: string;

  public
    constructor Create(ATable: TTable; AName: String; AAlias: String = '';
      AIsFuctionCol: Boolean = False); overload;
    function ToString: string; override;
    procedure Write(AOut: TOutput); override;
    procedure Write(AOut: TOutput; AOptions: TColumnOpts); overload; virtual;
    property Name: string read FName;
    property Alias : string read FAlias write FAlias;
    property Table: TTable read GetTable;
    property IsFunction: Boolean read FIsFunction write FIsFunction;
  end;

  TSubQueryColumn = class(TColumn)
  private
    FQuery: string;
  public
    constructor Create(AQuery: string; AAlias: string); overload;
    constructor Create(AQuery: TSelectQuery; AAlias: string); overload;
    procedure Write(AOut: TOutput; AOptions: TColumnOpts); override;
    property Query: string read FQuery;
  end;

  TTable = class (TOutputable)
  private
    FAlias: string;
    FName: string;
    function GetAlias: string;
  public
    constructor Create(Name: string); overload;
    constructor Create(Name: string; Alias: string); overload;
    function Equals(obj: TObject): Boolean;
    function GetColumn(AColumnName: string): TColumn;
    function HasAlias: Boolean;
    function ToString: string; override;
    procedure Write(AOut: TOutput); override;
    // The alias for the table.
    property Alias: string read GetAlias write FAlias;
    // The actual name of the table in the database.
    property Name: string read FName write FName;
  end;

  TOrder = class (TOutputable)
  private
    FAscending: Boolean;
    FColumn: TColumn;
  public
    constructor Create(Column: TColumn; Ascending: Boolean);
    function ToString: string; override;
    procedure Write(AOut: TOutput); override;
    property Column: TColumn read FColumn;
  end;

  TCriteria = class (TOutputable)
  protected
    function Quote(s:string): string;
  public
    function ToString: string; override;
    procedure Write(AOut: TOutput); override; abstract;
  end;

  {*
    With an instance of this class it is possible to generate an sql query.
  }
  TSelectQuery = class (TOutputable)
  private
    FBaseTable: TTable;

    FColumns: TObjectList;
    FJoins : TObjectList;
    FCriteria: TObjectList;
    FOrder: TObjectList;
    FGroupByCols : TList;

    FGroupByType : TGroupByType;

    FIndentSize: Integer;
    FUseNewLines : Boolean;

  public
    constructor Create(oBaseTable: TTable);
    destructor Destroy; override;

    procedure AddColumn(AColumn: TColumn; AGroup: Boolean = False); overload;
    function AddColumn(Table: TTable; ColumnName: String; AAlias: String = '';
      AGroup: Boolean = False): TColumn; overload;
    procedure AddColumn(AQuery: string; AAlias: String;
      AGroup: Boolean = False); overload;
    procedure AddColumns(Table: TTable; ColumnNames: TStringList); overload;
    procedure AddColumns(Table: TTable; ColumnNames: Array of String); overload;
    procedure AddCriteria(Criteria: TCriteria);
    procedure AddFunctionColumn(ATable: TTable; AFunctionName: string;
      AAlias: string = '';AGroup: Boolean = False);
    procedure AddJoin(srcTable: TTable; srcColumnName: string; destTable:
        TTable; destColumnName: string); overload;

    procedure AddJoin(AJoin: TJoin); overload;

    procedure AddJoin(destTable: TTable; SrcColumnName, DestColumnName: string;
        JoinType : TJoinType); overload;

    procedure AddJoin(ASourceTbl, ADestTbl: TTable;
        ASrcColName, ADestColName : string; JoinType: TJoinType); overload;

    procedure AddOrder(Order: TOrder); overload;
    procedure AddOrder(Table: TTable; ColumnName: string; Ascending : Boolean);
            overload;
    // Iterate through a list and append all entries (using .ToString()) to a
    // stringbuffer.
    procedure AppendList(AOut: TOutput; list: TList; Seperator: string);
    procedure AppendColumnList(AOut:TOutput; list: TList; Seperator: string;
      AOptions: TColumnOpts = [coName, coAlias]);
    function findAllUnJoinedTables: TList; overload;
    function FindColumnByAlias(AName: String): TColumn;
    function IsTableJoined(Table: TTable): Boolean;
    function ListColumns: TList;
    function ListCriteria: TList;
    function ListOrder: TList;

    // A list containing the number of
    //property UsedTables : TObjectList read FUsedTables;
//    function AddTableRef(AName: String): Integer;
//    function GetTableRef(AName: String): Integer;

    procedure RemoveColumn(Column: TColumn);
    procedure RemoveCriteria(Criteria: TCriteria);
    procedure RemoveOrder(Order: TOrder);
    function ToString: string; override;
    procedure Write(AOut: TOutput); override;
    property BaseTable: TTable read FBaseTable;
    property IndentSize: Integer read FIndentSize write FIndentSize default 4;
//    property Table: TTable read FTable;
    property UseNewLines: Boolean read FUseNewLines write FUseNewLines default true;
    property GroupByType : TGroupByType read FGroupByType write FGroupByType
      default gbtDefault;

    property SQL: String read ToString;
    property Joins: TObjectList read FJoins;
  end;

  TJoin = class(TOutputable)
  private
    FSource,
    FDest : TColumn;
    FJoinType : TJoinType;
  public
    constructor Create(source: TColumn; dest: TColumn); overload;
    constructor Create(source: TColumn; dest: TColumn; JoinType: TJoinType);
        overload;
    function ToString: string; override;
    procedure Write(AOut: TOutput); override;
  end;

  TBaseLogicGroup = class (TCriteria)
  private
    FList : TList;
    FOperator: string;
  public
    constructor Create(operator: string; left: TCriteria; right: TCriteria); overload;
    constructor Create(operator: string; list: TList); overload;
    procedure Write(AOut: TOutput); override;
  end;

  TAnd = class (TBaseLogicGroup)
  public
    constructor Create(left: TCriteria; right: TCriteria); overload;
    constructor Create(list: TList); overload;
  end;

  TOr = class (TBaseLogicGroup)
  public
    constructor Create(left: TCriteria; right: TCriteria); overload;
    constructor Create(list: TList); overload;
  end;

  TInCriteria = class (TCriteria)
  private
    FColumn: TColumn;
    FSubSelect: TSelectQuery;
    FValue: string;
  public
    constructor Create(column: TColumn; SubSelect: String); overload;
    constructor Create(column: TColumn; const values: array of const); overload;
    constructor Create(column: TColumn; values: TList); overload;
    constructor Create(column: TColumn; SubSelect: TSelectQuery); overload;
    constructor Create(table: TTable; columnName: string; subSelect: string);
            overload;
    constructor Create(table:TTable; columnName: string; const values: array of
            const); overload;
    constructor Create(table: TTable; columnName: string; values: TList);
            overload;
    constructor Create(table: TTable; ColumnName: string; SubSelect:
            TSelectQuery); overload;
    procedure Write(AOut: TOutput); override;
    property Column: TColumn read FColumn;
  end;

  TJoinCriteria = class (TCriteria)
  private
    FDest: TColumn;
    FSource: TColumn;
  public
    constructor Create(source : TColumn; dest: TColumn);
    procedure Write(AOut: TOutput); override;
    property Dest: TColumn read FDest;
    property Source: TColumn read FSource;
  end;

  TMatchCriteria = class (TCriteria)
  private
    FColumn: TColumn;
    FMatchType: string;
    FValue: string;
    FDBDateTimeFormat : string;
  public
    constructor Create(column: TColumn; matchType: string; value: Variant);
            overload;
    constructor Create(column: TColumn; matchType: string; value: Boolean);
            overload;
    constructor Create(column: TColumn; matchType: string; value: Integer);
            overload;
    constructor Create(column: TColumn; matchType: string; var value: Real);
            overload;
    constructor Create(column: TColumn; matchType: string; value: string);
            overload;
// This constructor conflicts with the Real version.
    constructor Create(column: Tcolumn; matchType: string; var value: TDateTime);
            overload;

    constructor Create(table: TTable; columnName : string; matchType: string;
            value: Variant); overload;
    constructor Create(table: TTable; columnName : string; matchType: string;
            value: Boolean); overload;
    constructor Create(table: TTable; columnName: string; matchType:string;
            value: Integer); overload;
    constructor Create(table: TTable; columnName : string; matchType: string;
            var value: Real); overload;
    constructor Create(table: TTable; columnName : string; matchType: string;
            value: string); overload;
// This constructor conflicts with the Real version.
    constructor Create(table: TTable; columnName : string; matchType: string;
            var value: TDateTime); overload;

    procedure Write(AOut: TOutput); override;
    property Column: TColumn read FColumn;
    property DBDateTimeFormat : string read FDBDateTimeFormat
        write FDBDateTimeFormat;
  end;

  TWildCardColumn = class (TColumn)
  public
    constructor Create(Table: TTable);
  end;

  TToStringer = class (TObject)
  public
    class function ToString(Outputable: TOutputable): string;
  end;

function DBDateStr(ADate : TDateTime) : String;
function DBTimeStr(ADate : TDateTime) : String;
function DBDateTimeStr(ADate : TDateTime) : String;

function ToDB(var AValue : Double) : string; overload;
function ToDB(var AValue : TDateTime) : string; overload;

var
  LastQuerySQLString : String;

implementation

uses StrUtils, Variants;

function ToDB(var AValue : Double) : string;
begin
  Result := FloatToStr(AValue);
end;

function ToDB(var AValue : TDateTime) : string;
begin
  Result := DBDateTimeStr(AValue);
end;

function DBDateStr(ADate : TDateTime) : String;
begin
  Result := '{ d''' + FormatDateTime('yyyy-mm-dd', ADate) + ''' }';
end;

function DBTimeStr(ADate : TDateTime) : String;
begin
  Result := '{ t''' + FormatDateTime('hh:nn:ss.zzz', ADate) + ''' }';
end;

function DBDateTimeStr(ADate : TDateTime) : String;
begin
  Result := '{ ts''' + FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', ADate) + ''' }';
end;

constructor TColumn.Create(ATable: TTable; AName: String; AAlias : String = '';
  AIsFuctionCol: Boolean = False);
begin
  FTable := ATable;
  FName := AName;
  FAlias := AAlias;
  FIsFunction := AIsFuctionCol;
end;

function TColumn.GetTable: TTable;
begin
  result := FTable;
end;

function TColumn.ToString: string;
begin
  Result := TToStringer.ToString(self);
end;

procedure TColumn.Write(AOut: TOutput);
begin
  Write(AOut, [coName, coAlias]);
end;

procedure TColumn.Write(AOut: TOutput; AOptions: TColumnOpts);
begin
  if IsFunction then
    AOut.Write(FName)
  else
  if (coName in AOptions) or (FAlias = '') then
    AOut.Write(FTable.Alias)
        .Write('.')
        .Write(FName);

  if (coAlias in AOptions) and (FAlias <> '') then
    AOut.Write(' ' + FAlias);
end;

{
function TSelectQuery.AddTableRef(AName: String): Integer;
var
  Idx : Integer;
begin
  Idx := FTableRefs.IndexOf(AName);
  if Idx <> -1 then
    FTableRefs.Objects[Idx] := TObject(Integer(FTableRefs.Objects[Idx])+1)
  else
    FTableRefs.AddObject(AName, TObject(Idx));
end;
}

{
  @returns the number of times the string is used as a table.
}
{
function TSelectQuery.GetTableRef(AName: String): Integer;
var
  Idx : Integer;
begin
  Idx := FTableRefs.IndexOf(AName);
  if Idx <> -1 then
    Result := Integer(FTableRefs.Objects[Idx])
  else
    Result := -1;
end;
}

{ TRefTable }

{
  Increase the reference count for the assiciated object.
}
procedure TRefTable.IncRef;
begin
  inc(FRef);
end;

{
  Initialize the reference to 0 and the table field to the given table;
}
constructor TRefTable.Create(ATable: TTable);
begin
  FRef := 0;
  FTable := ATable;
end;

{ TSubQueryColumn }

constructor TSubQueryColumn.Create(AQuery, AAlias: string);
begin
  FQuery := AQuery;
  FName := '';
  FAlias := AAlias;
end;

constructor TSubQueryColumn.Create(AQuery: TSelectQuery; AAlias: string);
begin
  FQuery := '(' + AQuery.ToString + ')';
  FName := '';
  FAlias := AAlias;

end;

procedure TSubQueryColumn.Write(AOut: TOutput; AOptions: TColumnOpts);
begin
  if coName in AOptions then
    AOut.Write(FQuery);

  if (coAlias in AOptions) and (FAlias <> '') then
    AOut.Write(' ' + FAlias);
end;

constructor TTable.Create(Name: string);
begin
  FName := Name;
end;

constructor TTable.Create(Name: string; Alias: string);
begin
  FName := Name;
  FAlias := Alias;
end;

{
  Determines of the given object is equal to this object.

  @returns true if the object is a table with the same alias.
}
function TTable.Equals(obj: TObject): Boolean;
begin
  if (obj = nil) or not (obj is TTable) then
      Result := False
  else
      Result := (Alias = TTable(obj).Alias);
end;

{
  @return string containing the alias of the table. If there was no alias
          defined the table name was returned.
}
function TTable.GetAlias: string;
begin
  if (HasAlias()) then
      Result := FAlias
  else
      Result := FName;
end;

{
  Get a TColumn object for the given name.

  The table does not internally cache columns. This means that two subsequent
  calls of GetColumn will return two different instences of the TColumn class.

  @param AColumnName a string containing the name of a column.

  @return A TColumn object representing a column with given name.
}
function TTable.GetColumn(AColumnName: string): TColumn;
begin
  Result := TColumn.Create(self, AColumnName);
end;

function TTable.HasAlias: Boolean;
begin
  Result := Trim(FAlias) <> '';
end;

function TTable.ToString: string;
begin
  Result := TToStringer.ToString(self);
end;

procedure TTable.Write(AOut: TOutput);
begin
  AOut.Write(Name);
  if (hasAlias()) then
      AOut.Write(' ' + Alias);
end;

constructor TSelectQuery.Create(oBaseTable: TTable);
begin
  indentSize := 4;

  FBaseTable := oBaseTable;
  FColumns := TObjectList.Create(true);
  FJoins := TObjectList.Create(true);
  FCriteria := TObjectList.Create(true);
  FOrder := TObjectList.Create(true);
  FGroupByCols := TList.Create;
end;

destructor TSelectQuery.Destroy;
begin
  FColumns.Free;
  FJoins.Free;
  FCriteria.Free;
  FOrder.Free;
  FGroupByCols.Free;
end;

procedure TSelectQuery.AddColumn(AColumn: TColumn; AGroup : Boolean);
begin
  FColumns.Add(AColumn);
  if AGroup then
    FGroupByCols.Add(AColumn);
end;

function TSelectQuery.AddColumn(Table: TTable; ColumnName: String;
  AAlias: String; AGroup: Boolean) : TColumn;
var
  Column : TColumn;
begin
  Column := Table.getColumn(ColumnName);
  Column.Alias := AAlias;
  AddColumn(Column, AGroup);
  Result := Column;
end;

procedure TSelectQuery.AddColumn(AQuery, AAlias: String; AGroup: Boolean);
begin
  AddColumn(TSubQueryColumn.Create(AQuery, AAlias), AGroup);
end;

{**
 Add a number of columns to the select statement.
}
procedure TSelectQuery.AddColumns(Table: TTable; ColumnNames: TStringList);
var
  I : Integer;
begin
  for I := 0 to pred(ColumnNames.Count) do
  begin
    AddColumn(Table, ColumnNames[I]);
  end;
end;

procedure TSelectQuery.AddColumns(Table: TTable; ColumnNames: Array of String);
var I : Integer;
begin
  for I := 0 to High(ColumnNames) do
  begin
    AddColumn(Table, ColumnNames[I]);
  end;
end;

procedure TSelectQuery.AddCriteria(Criteria: TCriteria);
begin
  self.FCriteria.Add(Criteria);
end;

{
  Add a TJoin object to the query.
}
procedure TSelectQuery.AddJoin(srcTable: TTable; srcColumnName: string;
        destTable: TTable; destColumnName: string);
begin

  AddCriteria(TJoinCriteria.Create(
              srcTable.GetColumn(srcColumnName),
              destTable.GetColumn(destColumnName)));
end;

procedure TSelectQuery.AddJoin(destTable: TTable; SrcColumnName,
  DestColumnName: string; JoinType : TJoinType);
begin
{
  // When Joining a table care must be taken because when a join refers to an
  // already existing table specifier the query is deemed invalid.

  // Herefore we need to check is the destination table is alreay used.
  // We need to add the destination table to a list.

  // Tables used in the table clauses are also used tables.
  Ref := AddTableRef(destTable.Name);
  if Ref = 0 then
  begin

  end;
}
  AddJoin(TJoin.Create(
      FBaseTable.GetColumn(SrcColumnName),
      destTable.GetColumn(destColumnName),
      JoinType));
end;

{
  Adds a new join to the table.
}
procedure TSelectQuery.AddJoin(ASourceTbl, ADestTbl: TTable;
        ASrcColName, ADestColName : string; JoinType: TJoinType);
begin
  AddJoin(TJoin.Create(
    ASourceTbl.GetColumn(ASrcColName),
    ADestTbl.GetColumn(ADestColName),
    JoinType));
end;

{
  Adds a new join to the table.
}
procedure TSelectQuery.AddJoin(AJoin: TJoin);
begin
  // AJoin.FSource
  // AJoin.FDest
  self.FJoins.Add(AJoin);
end;

procedure TSelectQuery.AddOrder(Order: TOrder);
begin
  self.FOrder.Add(Order);
end;

procedure TSelectQuery.AddOrder(Table: TTable; ColumnName: string; Ascending :
        Boolean);
begin
  AddOrder(TOrder.Create(Table.GetColumn(ColumnName), Ascending));
end;

// Iterate through a list and append all entries (using .ToString()) to a stringbuffer.
procedure TSelectQuery.AppendList(AOut: TOutput; list: TList; Seperator: string);
var
  i: Integer;
begin
  for i := 0 to (List.Count -1) do
  begin
    TOutputable(List.Items[i]).Write(AOut);
    if i < List.Count-1 then
      AOut.Write(Seperator);
    AOut.WriteLn();
  end;
end;

{
  Determines if the table is used in the query
}
function TSelectQuery.IsTableJoined(Table: TTable): Boolean;
var
  Join : TJoin;
  I : Integer;
begin
  result := False;
  if Table = nil then exit;
  for I := 0 to FJoins.Count-1 do
  begin
    Join := TJoin(FJoins[I]);
    if Join.FDest.Table.Name = Table.Name then
    begin
      Result := True;
      exit;
    end
  end;
end;

function TSelectQuery.findAllUnJoinedTables: TList;
var
  AllTables: TList;
  Column: TColumn;
  Table: TTable;
  Criteria: TCriteria;
  I: Integer;
begin
  AllTables := TList.Create();

  AllTables.Add(FBaseTable);

  // Get all tables used by columns
  for I := 0 to FColumns.Count-1 do
  begin
    Column := TColumn(FColumns[I]);
    // Table may not be added if it is referred from, from one of the join
    // criteria.
    if (not IsTableJoined(Column.Table)) and
      (not (Column.Table = nil)) and (AllTables.IndexOf(Column.Table) = -1) then
      AllTables.Add(Column.Table);
  end;

  // Get all tables used by criteria
  for I := 0 to FCriteria.Count-1 do
  begin
    Criteria := TCriteria(FCriteria[I]);
    if Criteria is TJoinCriteria then
    begin
      try
        Table := TJoinCriteria(Criteria).Source.Table;
        if AllTables.IndexOf(Table) = -1 then
          AllTables.Add(Table);

        Table := TJoinCriteria(criteria).Dest.Table;
        if AllTables.IndexOf(Table) = -1 then
          AllTables.Add(Table);
      except
          // not a TJoinCriteria
      end;
    end;
  end;

    // Get all tables used by column ordering
  for I := 0 to FOrder.Count-1 do
  begin
    //Table := TOrder(FOrder[I]).Column.Table;
    Column := TOrder(FOrder[I]).Column;
    // Table may not be added if it is referred from, from one of the join
    // criteria.
    if (not IsTableJoined(Column.Table)) and
      (not (Column.Table = nil)) and (AllTables.IndexOf(Column.Table) = -1) then
      AllTables.Add(Column.Table);
  end;

  Result := AllTables;
end;

function TSelectQuery.ListColumns: TList;
begin
  // Should be unmodifiable
  Result := TList.Create();
  try
    Result.Assign(FColumns);
  except
  end;
end;

function TSelectQuery.ListCriteria: TList;
begin
  // return list should be unmodifiable.
  Result := TList.Create();
  try
    Result.Assign(FCriteria);
  except
  end;
end;

function TSelectQuery.ListOrder: TList;
begin
  // Should be unordered list
  Result := TList.Create();
  try
    Result.Assign(FOrder);
  except
  end;
end;

procedure TSelectQuery.RemoveColumn(Column: TColumn);
begin
  FColumns.Remove(Column);
end;

procedure TSelectQuery.RemoveCriteria(Criteria: TCriteria);
begin
  Self.FCriteria.Remove(Criteria);
end;

procedure TSelectQuery.RemoveOrder(Order: TOrder);
begin
  Self.FOrder.Remove(Order);
end;

function TSelectQuery.ToString: string;
var
  fs : TFileStream;

  procedure DumpStr(AStr: String);
  begin
    fs.Write(PChar(AStr)^, Length(AStr));
  end;

begin
  Result := TToStringer.toString(Self);
{$IFDEF LOG_SQL}
  fs := TFileStream.Create('SQL.log', fmCreate);
  fs.Seek(0, 2);
  try
    DumpStr('---------------------------------------------' + #$A#$D);
    DumpStr(Result + #$A#$D);
  finally
    FreeAndNil(fs);
  end;
{$ENDIF}
  LastQuerySQLString := Result;
end;

{
  Write the sql statement to the output class.

  @param AOut a TOuput class
}
procedure TSelectQuery.Write(AOut: TOutput);
begin
  if Not UseNewLines then
  begin
    AOut.FLineBreak := '';
    AOut.FIndent := ' ';
  end;

  AOut.WriteLn('SELECT');

  // Add columns to the select
  AOut.indent;
  if FColumns.Count = 0 then
    AOut.Write('*')
  else
    AppendColumnList(AOut, FColumns, ',', [coName, coAlias]);
  AOut.unindent;

  // Add tables to select from
  AOut.WriteLn(' FROM');

  // Determine all tables used in the query, but not used the join clauses
  AOut.indent;
  AppendList(AOut, findAllUnJoinedTables, ',');
  AOut.unindent;

  AOut.indent;
  AppendList(AOut, FJoins, ' ');
  AOut.unindent;

  // Add criteria
  if (FCriteria.Count > 0) then
  begin
    AOut.WriteLn(' WHERE');
    AOut.indent;
    AppendList(AOut, FCriteria, ' AND ');
    AOut.unindent;
  end;

  // Add Group by 
  if (FGroupByCols.Count > 0) then
  begin
    AOut.Write(' GROUP BY');
    if FGroupByType = gbtAll then
      AOut.WriteLn(' ALL')
    else
      AOut.WriteLn;

    AOut.indent;
    AppendColumnList(AOut, FGroupByCols, ',', [coName]);
    AOut.unindent;
  end;

  // Add ordering
  if (FOrder.Count > 0) then
  begin
    AOut.writeln(' ORDER BY');
    AOut.indent;
    AppendList(AOut, FOrder, ',');
    AOut.unindent;
  end;
end;

constructor TJoin.Create(source: TColumn; dest: TColumn);
begin
  FSource := source;
  FDest := dest;
  FJoinType := JOIN;
end;

constructor TJoin.Create(source: TColumn; dest: TColumn; JoinType: TJoinType);
begin
  FSource := source;
  FDest := dest;
  FJoinType := JoinType;
end;

function TJoin.ToString: string;
begin
  Result := TToStringer.toString(Self);
end;

procedure TJoin.Write(AOut: TOutput);
begin
  case FJoinType of
  INNER_JOIN:
    AOut.Write('INNER ');
  LEFT_JOIN:
    AOut.Write('LEFT ');
  RIGHT_JOIN:
    AOut.Write('RIGHT ');
  end;
  AOut.Write('JOIN ').Write(FDest.Table).Write(' ON ')
      .Write(FSource).Write(' = ').Write(FDest);
end;

constructor TAnd.Create(left: TCriteria; right: TCriteria);
begin
  inherited Create('AND', left, right);
end;

constructor TAnd.Create(list: TList); 
begin
  Inherited Create('AND', list);
end;

constructor TOr.Create(left: TCriteria; right: TCriteria);
begin
  Inherited Create('OR', left, right);
end;

constructor TOr.Create(list: TList);
begin
  Inherited Create('OR', list);
end;


constructor TBaseLogicGroup.Create(operator: string; left: TCriteria; right:
        TCriteria);
begin
  FList := TList.Create;
  FList.Add(left);
  FList.Add(right);
  FOperator := operator;
end;

constructor TBaseLogicGroup.Create(operator: string; list: TList);
begin
  FList := TList.Create;
  FList.Assign(list);
  FOperator := operator;
end;

{
  @param AOut Output object class.
}
procedure TBaseLogicGroup.Write(AOut: TOutput);
var
  I : Integer;
begin
  if (FList.Count = 0) then exit;

  AOut.Write('(');
  AOut.Write(TCriteria(fList.Items[0]));
  for I := 1 to Pred(FList.Count) do
  begin
    AOut.Write(' ').Write(FOperator).Write(' ');
    AOut.Write(TCriteria(Flist.Items[I]));
  end;
  AOut.Write(')');
end;

function TCriteria.Quote(s:string): string;
begin
  if (s = '') then
      Result := 'null'
  else
  begin
    Result := QuotedStr(s);
  end;
end;

function TCriteria.ToString: string;
begin
  Result := TToStringer.ToString(self);
end;

constructor TInCriteria.Create(column: TColumn; SubSelect: String);
begin
  FColumn := column;
  FValue := SubSelect;
end;

constructor TInCriteria.Create(column: TColumn; const values: array of const);
var
  str: TStringStream;
  i: Integer;
begin
  self.FColumn := column;
  str := TStringStream.Create('');
  try
    for i := 0 to Length(values)-1 do
    begin
        with values[i] do
        begin
          case VType of
            vtInteger:
              str.WriteString(IntToStr(VInteger));
            vtExtended:
              str.WriteString(FloatToStr(VExtended^));
            vtString:
              str.WriteString(quote(VString^));
            vtAnsiString:
              str.WriteString(quote(string(VAnsiString)));
          end;
        end;
        if (i < Length(values) - 1) then
            str.WriteString(',');
    end;
    Self.FValue := str.datastring;
  finally
    str.Free;
  end;
end;

constructor TInCriteria.Create(column: TColumn; values: TList);
var
  str: TStringStream;
  i: Integer;
  curr: TOutputable;
begin
  FColumn := Column;
  str := TStringStream.Create('');
  try
    for i := 0 to Values.Count-1 do
    begin
      curr := TOutputable(Values[i]);
      //if (curr instanceof Number) then
      //  str.WriteString(NumberToString(curr));
      //else
        str.WriteString(quote(curr.toString()));
      if (i < Values.Count-1) then
        str.WriteString(',');
    end;
    self.FValue := str.DataString;
  finally
    str.Free;
  end;
end;

constructor TInCriteria.Create(column: TColumn; SubSelect: TSelectQuery);
begin
  FColumn := column;
  FSubSelect := SubSelect;
end;

constructor TInCriteria.Create(table: TTable; columnName: string; subSelect: 
        string);
begin
  self.Create(table.getColumn(columnName), subSelect);
end;

constructor TInCriteria.Create(table:TTable; columnName: string; const values: 
        array of const);
begin
  self.Create(table.getColumn(columnName), values);
end;

constructor TInCriteria.Create(table: TTable; columnName: string; values: 
        TList);
begin
  self.Create(table.getColumn(columnName), values);
end;

constructor TInCriteria.Create(table: TTable; ColumnName: string; SubSelect: 
        TSelectQuery);
begin
  self.Create(table.getColumn(columnName), SubSelect);
end;

procedure TInCriteria.Write(AOut: TOutput);
begin
  AOut.Write(FColumn);
  AOut.WriteLn(' IN (');

  AOut.indent;
  if not (FSubSelect = nil) then
      FSubSelect.Write(AOut)
  else
      AOut.Write(FValue);
  AOut.unindent;

  AOut.WriteLn();
  AOut.Write(')');
end;

constructor TJoinCriteria.Create(source : TColumn; dest: TColumn);
begin
  FSource := source;
  FDest := Dest;
end;

procedure TJoinCriteria.Write(AOut: TOutput);
begin
  AOut.Write(FSource)
      .Write(' = ')
      .Write(FDest);
end;

constructor TMatchCriteria.Create(column: TColumn; matchType: string;
  value: Variant);
begin
  case VarType(Value) of
  varInteger:
    self.Create(column, matchType, integer(value));
  varString:
    self.Create(column, matchType, VarToStr(value));
  varDate:
    begin
      FColumn := column;
      FValue := DBDateTimeStr(value);
      FMatchType := matchType;
    end;
  varSingle:
    self.Create(column, matchType, single(value));
  varBoolean:
    self.Create(column, matchType, boolean(value));
  else
    self.Create(column, matchType, VarToStr(value));
  end;
end;

constructor TMatchCriteria.Create(column: TColumn; matchType: string; value:
        Boolean);
begin
  FColumn := column;
  FValue := BoolToStr(value);
  FMatchType := matchType;
end;

constructor TMatchCriteria.Create(column: TColumn; matchType: string; value:
        Integer);
begin
  FColumn := column;
  FValue := IntToStr(value);
  FMatchType := matchType;
end;

constructor TMatchCriteria.Create(column: TColumn; matchType: string;
    var value: Real);
begin
  FColumn := column;
  FValue := FloatToStr(value);
  FMatchType := matchType;
end;

constructor TMatchCriteria.Create(column: TColumn; matchType: string; value:
        string);
begin
  FColumn := column;
  FValue := Quote(value);
  FMatchType := matchType;
end;


constructor TMatchCriteria.Create(column: TColumn; matchType: string;
  var value: TDateTime);
begin
  FColumn := column;
  FValue := FormatDateTime(FDBDateTimeFormat, value);
  FMatchType := matchType;
end;


constructor TMatchCriteria.Create(table: TTable; columnName : string;
        matchType: string; value: Variant);
begin
  self.Create(table.getColumn(columnName), matchType, value);
end;

constructor TMatchCriteria.Create(table: TTable; columnName : string;
        matchType: string; value: Boolean);
begin
  self.Create(table.getColumn(columnName), matchType, value);
end;

constructor TMatchCriteria.Create(table: TTable; columnName: string;
        matchType:string; value: Integer);
begin
  self.Create(table.getColumn(columnName), matchType, value);
end;

constructor TMatchCriteria.Create(table: TTable; columnName : string;
        matchType: string; var value: Real);
begin
  self.Create(table.getColumn(columnName), matchType, TDateTime(value));
end;

constructor TMatchCriteria.Create(table: TTable; columnName : string;
        matchType: string; value: string);
begin
  self.Create(table.getColumn(columnName), matchType, value);
end;


constructor TMatchCriteria.Create(table: TTable; columnName,
  matchType: string; var value: TDateTime);
begin
  self.Create(table.getColumn(columnName), matchType, value);
end;

procedure TMatchCriteria.Write(AOut: TOutput);
begin
  AOut.Write(FColumn)
      .Write(' ')
      .Write(FMatchType)
      .Write(' ')
      .Write(FValue);
end;

constructor TOrder.Create(Column: TColumn; Ascending: Boolean);
begin
  FColumn := Column;
  FAscending := Ascending;
end;

function TOrder.ToString: string;
begin
  Result := TToStringer.ToString(self);
end;

procedure TOrder.Write(AOut: TOutput);
begin
  FColumn.Write(AOut, [coAlias]);
  if not FAscending then
     AOut.Write (' DESC');
end;

constructor TWildCardColumn.Create(Table: TTable);
begin
  inherited Create(Table, '*');
end;

// Constructor of this class
constructor TOutput.Create(indent: String);
begin
  FLineBreak := '';
  FIndent := indent;
  FResult := TStringStream.Create('');
  FCurrentIndent := '';
end;

constructor TOutput.Create(indent, LineBreak: string);
begin
  self.Create(indent);
  FLineBreak := LineBreak;
end;

destructor TOutput.Destroy;
begin
  FResult.Free;
  inherited;
end;

procedure TOutput.indent;
begin
  FCurrentIndent := FCurrentIndent + FIndent;
end;

function TOutput.ToString: string;
begin
  Result := FResult.DataString;
end;

procedure TOutput.unindent;
var
  n: Integer;
begin
  n := Length(FCurrentIndent) - Length(FIndent);
  FCurrentIndent := LeftStr(FCurrentIndent, n);
end;

function TOutput.Write(ch: Char): TOutput;
begin
  WriteNewLineIfNeeded;
  FResult.Write((@ch)^, 1); // FResult.WriteString(ch);
  Result := self;
end;

function TOutput.Write(s: string): TOutput;
begin
  WriteNewLineIfNeeded;
  FResult.WriteString(s);
  Result := self;
end;

function TOutput.Write(obj: TOutputable): TOutput;
begin
  Result := Write(obj.ToString);
end;

function TOutput.WriteLn: TOutput;
begin
  FNewLineComing := True;
  Result := self;
end;

function TOutput.WriteLn(s: string): TOutput;
begin
  Result := Write(s);
  FNewLineComing := True;
end;

function TOutput.WriteLn(obj: TOutputable): TOutput;
begin
  Result := Write(obj);
  FNewLineComing := True;
end;

procedure TOutput.WriteNewLineIfNeeded;
var
  s : String;
begin
  if FNewLineComing then
  begin
    s := FLineBreak + FCurrentIndent;
    FResult.WriteString(FLineBreak + FCurrentIndent);
    FNewLineComing := false;
  end
end;

class function TToStringer.ToString(Outputable: TOutputable): string;
var
  AOut: TOutput;
begin
  AOut := TOutput.Create('    ');
  outputable.Write(AOut);
  Result := AOut.ToString;
end;

function TSelectQuery.FindColumnByAlias(AName : String): TColumn;
var
  I : Integer;
begin

  for I := 0 To FColumns.Count-1 do
  begin
    if TColumn(FColumns[I]).Alias = AName then
    begin
      Result := TColumn(FColumns[I]);
      exit;
    end;
  end;
  Result := nil;
end;

procedure TSelectQuery.AppendColumnList(AOut: TOutput; list: TList;
  Seperator: string; AOptions : TColumnOpts);
var
  I : integer;
begin
  for i := 0 to (List.Count -1) do
  begin
    TColumn(List.Items[i]).Write(AOut, AOptions);
    if i < List.Count-1 then
      AOut.Write(Seperator);
    AOut.WriteLn();
  end;
end;

procedure TSelectQuery.AddFunctionColumn(ATable: TTable; AFunctionName,
  AAlias: string; AGroup: Boolean);
var
  Column : TColumn;
begin
  Column := TColumn.Create(ATable, AFunctionName, AAlias, True);
  Column.Alias := AAlias;
  AddColumn(Column, AGroup);
end;

end.
