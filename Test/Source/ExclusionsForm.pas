(**
  
  This module contains a clas to represent a form for editing the exclusions.

  @Version 1.0
  @Date    06 Mar 2009
  @Author  David Hoyle

**)
unit ExclusionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  (** A class to represent the form interface. **)
  TfrmExclusions = class(TForm)
    memExclusions: TMemo;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
  private
    { Private declarations }
  public
    { Public declarations }
    Class Procedure Execute(slExclusions : TStringList);
  end;

implementation

{$R *.dfm}

{ TfrmExclusions }

(**

  This is the forms main interface method for invoking the edit dialogue.

  @precon  slExclusions must be a valid instance of a string list.
  @postcon If the dialogue is confirmed the passed string list is updated with
           the edited list of exclusions.

  @param   slExclusions as a TStringList

**)
class procedure TfrmExclusions.Execute(slExclusions: TStringList);
begin
  With TfrmExclusions.Create(Nil) Do
    Try
      memExclusions.Lines.Assign(slExclusions);
      If ShowModal = mrOK Then
        slExclusions.Assign(memExclusions.Lines);
    Finally
      Free;
    End;
end;

end.
