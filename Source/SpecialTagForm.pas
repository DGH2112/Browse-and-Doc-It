(**
  
  This form provides a mean for editing Special tags names and description
  through a single method.

  @Author  David Hoyle
  @Version 1.0
  @Date    16 Nov 2008

**)
unit SpecialTagForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  (** Form for editing special tags **)
  TfrmSpecialTag = class(TForm)
    lblName: TLabel;
    edtName: TEdit;
    lblDescription: TLabel;
    edtDescription: TEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbxShowInTree: TCheckBox;
    cbxAutoExpand: TCheckBox;
    chkShowInDoc: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
    Class Function Execute(var strName, strDescription : String;
      var boolShowInTree, boolExpand, boolShowInDoc : Boolean) : Boolean;
  end;

implementation

{$R *.DFM}

{ TfrmSpecialTag }

(**

  This method accepts a name and description string and displays them for 
  editing in the form. If the user presses OK the form returns true. 

  @precon  strName is the tags name, strDescription is the tags description, 
           boolShow is a boolean value it indicate whether to show the tag in 
           the browser, boolExpand is a boolean value it indicate whether to 
           expand the tag in the browser and Returns true if the OK button 
           was pressed. 
  @postcon Displays the special tag form. 

  @param   strName        as a String as a reference
  @param   strDescription as a String as a reference
  @param   boolShowInTree as a Boolean as a reference
  @param   boolExpand     as a Boolean as a reference
  @param   boolShowInDoc  as a Boolean as a reference
  @return  a Boolean       

**)
class function TfrmSpecialTag.Execute(var strName, strDescription: String;
  var boolShowInTree, boolExpand, boolShowInDoc : Boolean): Boolean;

begin
  With TfrmSpecialTag.Create(Nil) Do
    Try
      Result := False;
      edtName.Text := strName;
      edtDescription.Text := strDescription;
      cbxShowInTree.Checked := boolShowInTree;
      cbxAutoExpand.Checked := boolExpand;
      chkShowInDoc.Checked := boolShowInDoc;
      If ShowModal = mrOK Then
        Begin
          strName := edtName.Text;
          strDescription := edtDescription.Text;
          boolShowInTree := cbxShowInTree.Checked;
          boolExpand :=cbxAutoExpand.Checked;
          boolShowInDoc := chkShowInDoc.Checked;
          Result := True
        End;
    Finally
      Free;
    End;
end;

end.
