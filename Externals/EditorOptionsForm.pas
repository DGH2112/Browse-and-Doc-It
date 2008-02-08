(**

  This module contains a for editing the visual, behavioural and highlighting
  properties of a currently conifgured TSynEdit editor.

  @Version 1.0
  @Author  David Hoyle
  @Date    08 Feb 2008

**)
unit EditorOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, SynEdit, CheckLst, Contnrs;

type
  (** This class represents the form interface for editing the editor
      options. **)
  TfrmEditorOptions = class(TForm)
    PageControl1: TPageControl;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    VisualTab: TTabSheet;
    cbxActiveLineColour: TColorBox;
    lblActiveLineColour: TLabel;
    BehaviourTab: TTabSheet;
    cbxFontName: TComboBox;
    edtFontSize: TEdit;
    udEditorFontSize: TUpDown;
    chxLineNumbers: TCheckBox;
    edtRightEdge: TEdit;
    udRightEdgePosition: TUpDown;
    cbxRightEdgeColour: TColorBox;
    cbxSelectedForeground: TColorBox;
    cbxSelectedBackground: TColorBox;
    SyntaxTab: TTabSheet;
    lblEditorFontName: TLabel;
    lblEditorFontSize: TLabel;
    lblRightEdgePosition: TLabel;
    lblRightEdgeColour: TLabel;
    lblForeColour: TLabel;
    lblBackColour: TLabel;
    clbOptions: TCheckListBox;
    cbxEditorBackgroundColour: TColorBox;
    lblEditorBackgroundColour: TLabel;
    lbAttributes: TListBox;
    cbxAttrForeColour: TColorBox;
    cbxAttrBackColour: TColorBox;
    lblAttrForeColour: TLabel;
    lblAttrBackColour: TLabel;
    grpFontStyles: TGroupBox;
    cbxBold: TCheckBox;
    cbxItalic: TCheckBox;
    cbxUnderlined: TCheckBox;
    cbxStrikeout: TCheckBox;
    lblTabWidth: TLabel;
    edtTabWidth: TEdit;
    udTabWidth: TUpDown;
    lblAttributes: TLabel;
    lblHighlighterType: TLabel;
    procedure lbAttributesClick(Sender: TObject);
    procedure AttributeChange(Sender: TObject);
  private
    { Private declarations }
    FAttributes : TObjectList;
    FUpdating : Boolean;
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Class Function Execute(Editor : TSynEdit) : Boolean;
  end;

implementation

{$R *.dfm}

Type
  (** This is a private class to hold information about each highlighter
      attribute. **)
  TAttribute = Class
  Private
    FForeColour : TColor;
    FBackColour : TColor;
    FStyle      : TFontStyles;
    FName       : String;
  Protected
  Public
    Constructor Create(Name : String; Fore, Back : TColor; Style : TFontStyles);
    (**
      This property reads and writes the Attribute Name
      @precon  None
      @postcon Reads and writes the Attribute Name
      @return  a String
    **)
    Property Name : String Read FName Write FName;
    (**
      This property reads and writes the Attribute ForeColour
      @precon  None
      @postcon Reads and writes the Attribute ForeColour
      @return  a TColor
    **)
    Property ForeColour : TColor Read FForeColour Write FForeColour;
    (**
      This property reads and writes the Attribute BackColour
      @precon  None
      @postcon Reads and writes the Attribute BackColour
      @return  a TColor
    **)
    Property BackColour : TColor Read FBackColour Write FBackColour;
    (**
      This property reads and writes the Attribute Style
      @precon  None
      @postcon Reads and writes the Attribute Style
      @return  a TFontStyles
    **)
    Property Style : TFontStyles Read FStyle Write FStyle;
  End;

  (** A record to describe descriptions for each TSynEditorOption. **)
  TSynEditorOptionsRecord = Record
    Description : String;
    Value : TSynEditorOption;
  End;

Const
  (** A const array of records to place descriptions agains each of the
      TSynEditorOptions. Use to provide an list of options. **)
  BehaviouralOptions : Array[Low(TSynEditorOption)..High(TSynEditorOption)] Of
    TSynEditorOptionsRecord = (
    (Description : '<Alt> key invokes Code Selection Mode'; Value: eoAltSetsColumnMode),
    (Description : 'Auto Indent'; Value: eoAutoIndent),
    (Description : 'Auto Size Max Scroll Width'; Value: eoAutoSizeMaxScrollWidth),
    (Description : 'Disable Scroll Arrows'; Value: eoDisableScrollArrows),
    (Description : 'Drag and Drop Editing'; Value: eoDragDropEditing),
    (Description : 'Drag and Drop Files'; Value: eoDropFiles),
    (Description : 'Enhanced Home Key'; Value: eoEnhanceHomeKey),
    (Description : 'Enhanced End key'; Value: eoEnhanceEndKey),
    (Description : 'Group Undo'; Value: eoGroupUndo),
    (Description : 'Half Page Scroll'; Value: eoHalfPageScroll),
    (Description : 'Hide and Show Scroll Bars'; Value: eoHideShowScrollbars),
    (Description : 'Keep Caret X'; Value: eoKeepCaretX),
    (Description : 'No Caret'; Value: eoNoCaret),
    (Description : 'No Selection'; Value: eoNoSelection),
    (Description : 'Right Mouse Moves Cursor'; Value: eoRightMouseMovesCursor),
    (Description : 'Scroll By One Less'; Value: eoScrollByOneLess),
    (Description : 'Scroll Hint Follows'; Value: eoScrollHintFollows),
    (Description : 'Scroll Past End of File'; Value: eoScrollPastEof),
    (Description : 'Scroll Past End of Line'; Value: eoScrollPastEol),
    (Description : 'Show Scroll Hints'; Value: eoShowScrollHint),
    (Description : 'Show Special Characters'; Value: eoShowSpecialChars),
    (Description : 'Smart Tab Delete'; Value: eoSmartTabDelete),
    (Description : 'Smart Tabs'; Value: eoSmartTabs),
    (Description : 'Special Line Default FG'; Value: eoSpecialLineDefaultFg),
    (Description : 'Tab Indent'; Value: eoTabIndent),
    (Description : 'Tabs to Spaces'; Value: eoTabsToSpaces),
    (Description : 'Trim Trailing Spaces'; Value: eoTrimTrailingSpaces)
  );

(**

  This is the constructor method for the TAttribute class.

  @precon  None.
  @postcon Creates an instance of a TAttribute class.

  @param   Name  as a String
  @param   Fore  as a TColor
  @param   Back  as a TColor
  @param   Style as a TFontStyles

**)
Constructor TAttribute.Create(Name : String; Fore, Back : TColor;
  Style : TFontStyles);

Begin
  FName := Name;
  FForeColour := Fore;
  FBackColour := Back;
  FStyle := Style;
End;

(**

  This is the constructor method for the TfrmEditorOptions class.

  @precon  None.
  @postcon Creates an instance of the form and initailises the interface
           controls.

  @param   AOwner as a TComponent

**)
Constructor TfrmEditorOptions.Create(AOwner : TComponent);

Var
  i : Integer;
  j : TSynEditorOption;

Begin
  Inherited Create(AOwner);
  FAttributes := TObjectList.Create(True);
  For i := 0 To Screen.Fonts.Count - 1 Do
    cbxFontName.Items.Add(Screen.Fonts[i]);
  For j := Low(TSynEditorOption) To High(TSynEditorOption) Do
    clbOptions.Items.Add(BehaviouralOptions[j].Description);
End;

(**

  This is the destructor method for the TfrmEditorOptions class.

  @precon  None.
  @postcon Frees the memory used fo the highlighter attributes.

**)
Destructor TfrmEditorOptions.Destroy;

Begin
  FAttributes.Free;
  Inherited Destroy;
End;

(**

  This is the forms main interface method.

  @precon  Editor must be a valid instance of a TSynEdit cvontrol.
  @postcon nvokes a form for editing the given instance of the TSynEdit control.

  @param   Editor as a TSynEdit as a reference
  @return  a Boolean

**)
Class Function TfrmEditorOptions.Execute(Editor : TSynEdit) : Boolean;

Var
  i : TSynEditorOption;
  j : Integer;

Begin
  Result := False;
  With TfrmEditorOptions.Create(Nil) Do
    Try
      // Visual
      udTabWidth.Position := Editor.TabWidth;
      cbxEditorBackgroundColour.Selected := Editor.Color;
      cbxActiveLineColour.Selected := Editor.ActiveLineColor;
      cbxFontName.Text := Editor.Font.Name;
      udEditorFontSize.Position := Editor.Font.Size;
      chxLineNumbers.Checked := Editor.Gutter.ShowLineNumbers;
      udRightEdgePosition.Position := Editor.RightEdge;
      cbxRightEdgeColour.Selected := Editor.RightEdgeColor;
      cbxSelectedForeground.Selected := Editor.SelectedColor.Foreground;
      cbxSelectedBackground.Selected := Editor.SelectedColor.Background;
      // Behavioural
       For i := Low(TSynEditorOption) To High(TsynEditorOption) Do
        clbOptions.Checked[Integer(i)] := i In Editor.Options;
      // Highlighter
      If Editor.Highlighter <> Nil Then
        Begin
          lblHighlighterType.Caption := Editor.Highlighter.LanguageName;
          For j := 0 To Editor.Highlighter.AttrCount - 1 Do
            With Editor.Highlighter.Attribute[j] Do
              Begin
                FAttributes.Add(TAttribute.Create(Name, Foreground, Background,
                  Style));
                lbAttributes.Items.Add(Editor.Highlighter.Attribute[j].Name);
              End;
        End Else
          SyntaxTab.Enabled := False;
      // Initialise the Highlighter Attributes.
      If lbAttributes.Items.Count > 0 Then
        Begin
          lbAttributes.ItemIndex := 0;
          lbAttributesClick(Nil);
        End;
      If ShowModal = mrOK Then
        Begin
          // Visual
          Editor.TabWidth := udTabWidth.Position;
          Editor.Color := cbxEditorBackgroundColour.Selected;
          Editor.ActiveLineColor := cbxActiveLineColour.Selected;
          Editor.Font.Name := cbxFontName.Text;
          Editor.Font.Size := udEditorFontSize.Position;
          Editor.Gutter.Font.Name := cbxFontName.Text;
          Editor.Gutter.Font.Size := udEditorFontSize.Position;
          Editor.Gutter.ShowLineNumbers := chxLineNumbers.Checked;
          Editor.RightEdge := udRightEdgePosition.Position;
          Editor.RightEdgeColor := cbxRightEdgeColour.Selected;
          Editor.SelectedColor.Foreground := cbxSelectedForeground.Selected;
          Editor.SelectedColor.Background := cbxSelectedBackground.Selected;
          // Behavioural
           For i := Low(TSynEditorOption) To High(TsynEditorOption) Do
            If clbOptions.Checked[Integer(i)] Then
              Editor.Options := Editor.Options + [i]
            Else
              Editor.Options := Editor.Options - [i];
          // Highlighter
          If Editor.Highlighter <> Nil Then
            For j := 0 To Editor.Highlighter.AttrCount - 1 Do
              With Editor.Highlighter.Attribute[j] Do
                Begin
                  Foreground := (FAttributes[j] As TAttribute).ForeColour;
                  Background := (FAttributes[j] As TAttribute).BackColour;
                  Style      := (FAttributes[j] As TAttribute).Style;
                End;
          Result := True;
        End;
    Finally
      Free;
    End;
End;

(**

  This is the attribute list box's on click event handler.

  @precon  None.
  @postcon Updates the attribute conotrols with the selected attribute in the
           list box.

  @param   Sender as a TObject

**)
procedure TfrmEditorOptions.lbAttributesClick(Sender: TObject);

Var
  i : Integer;
  A : TAttribute;

begin
  If lbAttributes.ItemIndex > -1 Then
    For i :=  0 To FAttributes.Count - 1 Do
      Begin
        A := FAttributes[i] As TAttribute;
        If A.Name = lbAttributes.Items[lbAttributes.ItemIndex] Then
          Begin
            FUpdating := True;
            Try
              cbxAttrForeColour.Selected := A.ForeColour;
              cbxAttrBackColour.Selected := A.BackColour;
              cbxBold.Checked := fsBold In A.Style;
              cbxItalic.Checked := fsItalic In A.Style;
              cbxUnderlined.Checked := fsUnderline In A.Style;
              cbxStrikeout.Checked := fsStrikeOut In A.Style;
            Finally
              FUpdating := False;
            End;
          End;
      End;
end;

(**

  This method is an on changes event handler for the attribute controls.

  @precon  None.
  @postcon Updates the attribute with the changes in the attribute controls.

  @param   Sender as a TObject

**)
procedure TfrmEditorOptions.AttributeChange(Sender: TObject);

Var
  i : Integer;

begin
  If lbAttributes.ItemIndex > -1 Then
    If Not FUpdating Then
      For i :=  0 To FAttributes.Count - 1 Do
        If (FAttributes[i] As TAttribute).Name =
          lbAttributes.Items[lbAttributes.ItemIndex] Then
          Begin
            (FAttributes[i] As TAttribute).ForeColour := cbxAttrForeColour.Selected;
            (FAttributes[i] As TAttribute).BackColour := cbxAttrBackColour.Selected;
            (FAttributes[i] As TAttribute).Style := [];
            If cbxBold.Checked Then
              (FAttributes[i] As TAttribute).Style :=
                (FAttributes[i] As TAttribute).Style + [fsBold];
            If cbxItalic.Checked Then
              (FAttributes[i] As TAttribute).Style :=
                (FAttributes[i] As TAttribute).Style + [fsItalic];
            If cbxUnderlined.Checked Then
              (FAttributes[i] As TAttribute).Style :=
                (FAttributes[i] As TAttribute).Style + [fsUnderline];
            If cbxStrikeout.Checked Then
              (FAttributes[i] As TAttribute).Style :=
                (FAttributes[i] As TAttribute).Style + [fsStrikeout];
          End;
end;

end.
