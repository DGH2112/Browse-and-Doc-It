unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxStyles, cxCustomData, cxGraphics, cxFilter, cxData,
  cxDataStorage, cxEdit, DB, cxDBData, StdCtrls, cxGridLevel, cxClasses,
  cxControls, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, ADODB, ActiveX, cxGridCustomPopupMenu,
  cxGridPopupMenu;

type
  TFrmMain = class(TForm)
    ADOConnection1: TADOConnection;
    Qry: TADOQuery;
    MainGrid: TcxGrid;
    Button1: TButton;
    cxGridPopupMenu1: TcxGridPopupMenu;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure OnGetStoredProperties(
      Sender: TcxCustomGridView; AProperties: TStrings);
    procedure OnGetStoredPropertyValue( Sender: TcxCustomGridView;
      const AName: String; var AValue: Variant);
    procedure OnSetStoredPropertyValue( Sender: TcxCustomGridView;
      const AName: String; const AValue: Variant);

  private
    { Private declarations }
  public
    { Public declarations }
    function GetSearchDataSet(sQry: string; AOpenClosed: Boolean = False): TDataSet;
  end;

var
  FrmMain: TFrmMain;

implementation

uses uBOMgr, uBOListDataSource, ComObj, uGridBldr73;

{$R *.dfm}

procedure TFrmMain.Button1Click(Sender: TObject);
var cid: integer;
    VwNm: string;
    stream: TMemoryStream;

  procedure LoadCustomGridViews(aLevel: TcxGridLevel);
  var
    idx,
    I: integer;
    AName: string;
  begin
    {$IFDEF debug}Log.Add('LoadCustomGridViews ' + ALevel.Name, DEBUG_LOG);
    {$ENDIF}
    for idx := 0 to pred(aLevel.Count) do
    begin
      aName := aLevel.Items[idx].GridView.Name;
      try
        // restore using the new way
        try
          {
            Use AChildrenCreating = True to make sure the footers are restored.
            This has as a resulting effect that deleted fields shown as empty columns in the grid.
            We will remove these later on. Issue reported to DevExpress.
          }
          aLevel.Items[idx].GridView.RestoreFromStream(
            Stream, True, False, [gsoUseFilter, gsoUseSummary]);
        except
        end;

        // Remove the columns we do not want.
        with TcxGridDBTableView(aLevel.Items[idx].GridView) do
        begin
          for I := ColumnCount -1 downto 0 do
            if (columns[I].Caption = '') then
              Columns[I].Free;
        end;
      except
      end;
      if aLevel.Items[idx].IsMaster then
        LoadCustomGridViews(aLevel.Items[idx]);
    end;
  end;

begin
  Qry.Open;
  cid := Qry.Fields[0].AsInteger;
  VwNm := BOMgr.BOList.GetByID(cid).Name + '_Search';
  FillGrid(cid, MainGrid, VwNm, '', True, True);

  stream := TMemoryStream.Create;
  try
    (Qry.Fields[1] as TBlobField).SaveToStream(stream);
    stream.Position := 0;
    LoadCustomGridViews(MainGrid.Levels);
  finally
    stream.Free;
  end;
  MainGrid.FocusedView.StoreToIniFile('c:\test.ini');
  Qry.Close;
end;

function TFrmMain.GetSearchDataSet(sQry: string;
  AOpenClosed: Boolean): TDataSet;
var
  Qry: TADOQuery;
begin
  {$IFDEF debug}Log.Add('DataMod - GetSearchDataSet - '+sQry); {$ENDIF}
  Qry := TADOQuery.Create(self);
  try
    Qry.Connection := ADOConnection1;
    Qry.SQL.Text := sQry;
    if not AOpenClosed then
      Qry.Open;
  finally
    Result := Qry;
  end;
end;

procedure TFrmMain.FormShow(Sender: TObject);
begin
  BOMgr.LoadModel;
end;

procedure TFrmMain.OnGetStoredProperties(Sender: TcxCustomGridView;
  AProperties: TStrings);
begin
  AProperties.Add('ShowHint');
  AProperties.Add('ShowHeaders');
  AProperties.Add('ShowGrid');
  AProperties.Add('AutoWidth');
  AProperties.Add('AutoHeight');
  AProperties.Add('AutoPreview');
end;

procedure TFrmMain.OnGetStoredPropertyValue(Sender: TcxCustomGridView;
  const AName: String; var AValue: Variant);
begin
  if (AName = 'ShowGrid') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      AValue := TcxGridTableView(Sender).OptionsView.GridLines;
      Exit;
    end;
  end;
  if (AName = 'AutoWidth') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      AValue := TcxGridTableView(Sender).OptionsView.ColumnAutoWidth;
      Exit;
    end;
  end;
  if (AName = 'AutoHeight') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      AValue := TcxGridTableView(Sender).OptionsView.CellAutoHeight;
      Exit;
    end;
  end;
  if (AName = 'AutoPreview') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      AValue := TcxGridTableView(Sender).Preview.Visible;
      Exit;
    end;
  end;
end;

procedure TFrmMain.OnSetStoredPropertyValue(Sender: TcxCustomGridView;
  const AName: String; const AValue: Variant);
begin
  if (AName = 'ShowGrid') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      TcxGridTableView(Sender).OptionsView.GridLines := AValue;
      Exit;
    end;
  end;
  if (AName = 'AutoWidth') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      TcxGridTableView(Sender).OptionsView.ColumnAutoWidth := AValue;
      Exit;
    end;
  end;
  if (AName = 'AutoHeight') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      TcxGridTableView(Sender).OptionsView.CellAutoHeight := AValue;
      Exit;
    end;
  end;
  if (AName = 'AutoPreview') then
  begin
    if (Sender is TcxGridTableView) then
    begin
      TcxGridTableView(Sender).Preview.Visible := AValue;
      Exit;
    end;
  end;
end;

initialization
  CoInitialize(nil);
  BOMgr := TBusinessObjectManager.Create;

finalization
  BOMgr.Free;
  CoUninitialize;

end.
