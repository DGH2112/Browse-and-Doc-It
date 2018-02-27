program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {FrmMain},
  uBOMgr in 'uBOMgr.pas',
  uAttribute in 'uAttribute.pas',
  uBOListDataSource in 'uBOListDataSource.pas',
  uSCMConst in 'uSCMConst.pas',
  uSQL in 'uSQL.pas',
  uClassMgr in 'uClassMgr.pas',
  uGridBldr73 in 'uGridBldr73.pas',
  uGridLayout in 'uGridLayout.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
