(**
  
  This module contains a clas to represent a form for editing the exclusions.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
