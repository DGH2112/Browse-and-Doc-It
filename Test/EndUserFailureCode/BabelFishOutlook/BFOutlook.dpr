library BFOutlook;

{$R 'resources.res' 'resources.rc'}

uses
  ComServ,
  BFOutlook_TLB in 'BFOutlook_TLB.pas',
  uAddin in 'uAddin.pas' {BabelFish: CoClass},
  AddInDesignerObjects_TLB in 'AddInDesignerObjects_TLB.pas',
  uOutlookEvents in 'uOutlookEvents.pas',
  uSaveClipboard in 'uSaveClipboard.pas',
  uParentedWnd in 'uParentedWnd.pas',
  fTranslate in 'fTranslate.pas' {dlgTranslate},
  dlgWait in 'dlgWait.pas' {WaitDlg},
  fPropetyPage in 'fPropetyPage.pas' {ToolsOptionsPage: TActiveForm} {ActiveFormX: CoClass},
  uBabelfishSOAP in 'uBabelfishSOAP.pas';

{$E dll}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
