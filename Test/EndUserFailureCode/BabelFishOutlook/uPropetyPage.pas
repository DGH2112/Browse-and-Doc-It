unit uPropetyPage;

interface

uses
  Windows, ActiveX, Classes, Controls, Graphics, Menus, Forms, StdCtrls,
  ComServ, StdVCL, AXCtrls, BFOutlook_TLB, ExtCtrls, Outlook_TLB;

type
  TToolsPropertyPage = class(TActiveForm{TActiveXControl}, IToolsPropertyPage, PropertyPage)
  private
    { Private declarations }
    FDelphiControl: TPanel;
  protected
    { Protected declarations }
    //procedure InitializeControl; override;
  public
    function  GetPageInfo(var HelpFile: WideString; var HelpContext: Integer): HResult; stdcall;
    function  Get_Dirty(out Dirty: WordBool): HResult; stdcall;
    function  Apply: HResult; stdcall;
  end;

implementation

uses ComObj;

{ TPropetyPage }

function TToolsPropertyPage.Apply: HResult;
begin
  Result:=S_OK;
end;

function TToolsPropertyPage.Get_Dirty(out Dirty: WordBool): HResult;
begin
  Dirty:=false;
  Result:=S_OK;
end;

function TToolsPropertyPage.GetPageInfo(var HelpFile: WideString;
  var HelpContext: Integer): HResult;
begin
  HelpFile:='';
  HelpContext:=0;
  Result:=S_OK;
end;

procedure TToolsPropertyPage.InitializeControl;
begin
  FDelphiControl := Control as TPanel;
  FDelphiControl.Caption:='BabelFish';
  FDelphiControl.BorderWidth:=0;
end;

initialization
  {TActiveXControlFactory.Create(
    ComServer,
    TToolsPropertyPage,
    TPanel,
    Class_ToolsPropertyPage,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);}
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TActiveFormX,
    Class_ActiveFormX,
    2,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmApartment);
end.
