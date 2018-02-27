unit uBOMgr;

interface

uses Windows, uAttribute, ADODB, Forms, SysUtils, Classes, Variants, DB,
     cxGrid, Dialogs, Controls, SyncObjs;

type
  TBusinessObjectManager = class
  private
    fBOList: TBOList;       // Class Model BO's met attribuut lijsten, niet geladen met data.
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadModel;
    function LoadGridColumnDefs(AClassID: Integer): String;
    property BOList: TBOList read fBOList;
  end;

var
  LogGuard : TCriticalSection;
  BOMgr: TBusinessObjectManager;

implementation

uses uBOListDataSource, Unit1;

{ TBusinessObjectManager }

constructor TBusinessObjectManager.Create;
begin
  {$IFDEF debug}Log.Add('BOMgr - Create'); {$ENDIF}
  inherited Create;
  LogGuard := TCriticalSection.Create;
  fBOList := TBOList.Create;
  fBOList.Name := 'ModelList';
end;

destructor TBusinessObjectManager.Destroy;
var
  I : Integer;
begin
  {$IFDEF debug}Log.Add('BOMgr - Destroy - ModelList'); {$ENDIF}
  for I := fBOList.Count-1 downto 0 do
  begin
    if fBOList.Item[I].RefCount > 1 then
    begin
    {$IFDEF debug}Log.Add(PChar('BO still '
      + IntToStr(fBOList.Item[I].RefCount) + ' times reffered')); {$ENDIF}
    end;
  end;
  fBOList.Free;
  FreeAndNil(LogGuard);
  inherited Destroy;
end;

(**

  Load the Business Model (all the BOs) from the database
   and create all the BOs with their Attributes

**)
function TBusinessObjectManager.LoadGridColumnDefs(
  AClassID: Integer): String;
var
  Query : TADOQuery;
begin
  Result := '';
  Query := TADOQuery.Create(nil);
  try
    with Query do
    begin
      Connection := FrmMain.ADOConnection1;
      SQL.Add('SELECT GridColumns FROM classes');
      SQL.Add('WHERE ID=' + IntToStr(AClassID));
      Open;
      if RecordCount > 0 then
        Result := Fields[0].AsString;
    end;
    Query.Close;
  finally
    Query.Free;
  end;
end;

procedure TBusinessObjectManager.LoadModel();
var WorkQuery,
    AttrQuery: TADOQuery;
    aBO: TBusinessObject;
    anAttribute: TAttribute;
    sl: TStringList;
begin
  {$IFDEF debug}Log.Add('BOMgr - Begin Load Model'); {$ENDIF}
  WorkQuery := TADOQuery.Create(Application);
  AttrQuery := TADOQuery.Create(Application);
  sl := TStringList.Create;
  try
    WorkQuery.Connection := FrmMain.ADOConnection1;
    AttrQuery.Connection := FrmMain.ADOConnection1;
    with WorkQuery do
    begin
      SQL.Clear;
      SQL.Add(
        'SELECT C.ID, C.Name, C.TblName, C.InActionBar, C.Icon '
          + 'FROM Classes C');

      Open;
      while not Eof do
      begin
        aBO := TBusinessObject.Create;
        aBO.ClassID := Fields.Fields[0].AsInteger;
        aBO.Name := Fields.Fields[1].AsString;
        aBO.TblName := Fields.Fields[2].AsString;

        AttrQuery.Close;
        AttrQuery.SQL.Text :=
          //      0   1     2         3        4     5        6
          'SELECT ID, Name, CompName, FldName, Type, FldSize, ClassID, '
          // 7          8           9       10      11
          + 'InQSearch, SearchFlds, ReqIns, ReqFin, Filter '
          + 'FROM Attributes WHERE Tbl = ' + IntToStr(aBO.ClassID);
//          + ' ORDER BY ID'; // remove Type for not sorting.

        AttrQuery.Open;
        AttrQuery.First;
        while not AttrQuery.Eof do
        begin
          anAttribute := nil;
          // Create the Attribute
          case TMetaType(AttrQuery.Fields[4].Value) of
            mtCallNumber:
              anAttribute := TCallNumberAttribute.Create(
                 AttrQuery.Fields[0].AsInteger,
                 AttrQuery.Fields[1].AsString,
                 AttrQuery.Fields[3].AsString);
            mtID:
              anAttribute := TIDAttribute.Create(
                 AttrQuery.Fields[0].AsInteger,
                 AttrQuery.Fields[1].AsString,
                 AttrQuery.Fields[3].AsString);
            mtInteger:
              anAttribute := TIntegerAttribute.Create(
                 AttrQuery.Fields[0].AsInteger,
                 AttrQuery.Fields[1].AsString,
                 AttrQuery.Fields[3].AsString);
            mtIntCalc:
              anAttribute := TIntCalcAttribute.Create(
                 AttrQuery.Fields[0].AsInteger,
                 AttrQuery.Fields[1].AsString,
                 AttrQuery.Fields[3].AsString);
            mtDouble:
              anAttribute := TDoubleAttribute.Create(
                 AttrQuery.Fields[0].AsInteger,
                 AttrQuery.Fields[1].AsString,
                 AttrQuery.Fields[3].AsString);
            mtCurrency:
              anAttribute := TCurrencyAttribute.Create(
                 AttrQuery.Fields[0].AsInteger,
                 AttrQuery.Fields[1].AsString,
                 AttrQuery.Fields[3].AsString);
            mtString:
              anAttribute := TStringAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString,
                AttrQuery.Fields[5].AsInteger);
            mtStringSelect:
              anAttribute := TStringSelectAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString,
                AttrQuery.Fields[5].AsInteger,
                sl,
                AttrQuery.Fields[8].AsString);
            mtStringCombo:
              anAttribute := TStringComboAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString,
                AttrQuery.Fields[5].AsInteger,
                sl,
                AttrQuery.Fields[8].AsString);
            mtDateTime:
              anAttribute := TDateTimeAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString);
            mtDate:
              anAttribute := TDateAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString);
            mtTime:
              anAttribute := TTimeAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString);
            mtTreeCombo:
              begin
                anAttribute := TTreeComboAttribute.Create(
                  AttrQuery.Fields[0].AsInteger,
                  AttrQuery.Fields[1].AsString,
                  AttrQuery.Fields[3].AsString,
                  AttrQuery.Fields[8].AsString);
              end;
            mtMemo:
              anAttribute := TMemoAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString);
            mtBOCombo:
              anAttribute := TBOAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString,
                AttrQuery.Fields[8].AsString,
                AttrQuery.Fields[11].AsString,
                AttrQuery.Fields[6].AsInteger);
            mtBOSearch:
              anAttribute := TBOSearchAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString,
                AttrQuery.Fields[8].AsString,
                AttrQuery.Fields[11].AsString,
                AttrQuery.Fields[6].AsInteger);
            mtBOTree:
              anAttribute := TBOTreeAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString,
                AttrQuery.Fields[8].AsString,
                AttrQuery.Fields[11].AsString,
                AttrQuery.Fields[6].AsInteger);
            mtBOList:
              // SearchFields contains the correct field of the destination
              // class now !!!
              anAttribute := TBOListAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[8].AsString,
                AttrQuery.Fields[6].AsInteger);
            mtHyperlink:
              anAttribute := THyperlinkAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString,
                AttrQuery.Fields[5].AsInteger);
            mtRadio:
              begin
                anAttribute := TRadioAttribute.Create(
                  AttrQuery.Fields[0].AsInteger,
                  AttrQuery.Fields[1].AsString,
                  AttrQuery.Fields[3].AsString,
                  sl);
              end;
            mtState:
              begin
                anAttribute := TStateAttribute.Create(
                  AttrQuery.Fields[0].AsInteger,
                  AttrQuery.Fields[1].AsString,
                  AttrQuery.Fields[3].AsString);
              end;
            mtUnknown:
              anAttribute := TUnknownAttribute.Create(
                AttrQuery.Fields[0].AsInteger,
                AttrQuery.Fields[1].AsString,
                AttrQuery.Fields[3].AsString);
          end;
          if (anAttribute <> nil) then
          begin
            anAttribute.InQuickSearch := AttrQuery.Fields[7].AsBoolean;
            anAttribute.RequiredAtInsert := AttrQuery.Fields[9].AsBoolean;
            anAttribute.RequiredAtFinish := AttrQuery.Fields[10].AsBoolean;
            if (AttrQuery.Fields[7].AsBoolean)
              and not (anAttribute.GetMetaType in [mtUnknown, mtBOList]) then
            begin
              if (aBO.StrQuickSearch = '') then
                aBO.StrQuickSearch := AttrQuery.Fields[3].AsString
              else
                aBO.StrQuickSearch := aBO.StrQuickSearch + ', '
                  + AttrQuery.Fields[3].AsString;
            end;

            // Add the attribute to the BO
            aBO.AddAttribute(anAttribute);
          end;
          AttrQuery.Next;
        end;


        fBOList.Add(aBO);
        Next;
      end;
      Close;
    end;
  finally
    WorkQuery.Free;
    AttrQuery.Free;
  end;
  {$IFDEF debug}
    Log.Add('BOMgr - End Load Model');
    Log.Add('Model loaded :');
  {$ENDIF}
end;

end.
