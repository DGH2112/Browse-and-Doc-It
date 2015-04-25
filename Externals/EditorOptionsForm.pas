(**

  This module contains a for editing the visual, behavioural and highlighting
  properties of a currently conifgured TSynEdit editor.

  @Version 1.0
  @Author  David Hoyle
  @Date    25 Apr 2015

**)
unit EditorOptionsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, SynEdit, CheckLst, Contnrs,
  SynEditHighlighter;

type
  (** This is a private class to hold information about each highlighter
      attribute. **)
  TAttribute = Class
  Private
    FForeColour : TColor;
    FBackColour : TColor;
    FStyle      : TFontStyles;
    FName       : String;
    FAttribute  : TSynHighlighterAttributes;
    FParent     : TSynCustomHighlighter;
  Protected
  Public
    Constructor Create(Name : String; Fore, Back : TColor; Style : TFontStyles;
      Attr : TSynHighlighterAttributes; Parent : TSynCustomHighlighter);
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
    (**
      This property reads and writes the referenced TSynHighlighterAttrubute
      @precon  None
      @postcon Reads and writes the referenced TSynHighlighterAttrubute
      @return  a TSynHighlighterAttributes
    **)
    Property Attribute : TSynHighlighterAttributes Read FAttribute Write FAttribute;
    (**
      This property reads and writes the Parent Highlighter
      @precon  None
      @postcon Reads and writes the Parent Highlighter
      @return  a TSynCustomHighlighter
    **)
    Property Parent : TSynCustomHighlighter Read FParent Write FParent;
  End;

  (** A class to represent a collection of attributes. **)
  TAttributes = Class
  Private
    FAttributes     : TObjectList;
    FProcessedAttrs : TList;
  Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add(Attr : TSynHighlighterAttributes;
      Parent : TSynCustomHighlighter) : TAttribute;
    Procedure Update(boolIncTags : Boolean);
  End;

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
    chkWordWrap: TCheckBox;
    procedure lbAttributesClick(Sender: TObject);
    procedure AttributeChange(Sender: TObject);
  private
    { Private declarations }
    FAttributes : TAttributes;
    FUpdating : Boolean;
    Procedure AddHighlighter(Highlighter : TSynCustomHighlighter);
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Class Function Execute(OwnerForm : TForm; Editor : TSynEdit;
      boolIncTag : Boolean) : Boolean;
  end;

implementation

{$R *.dfm}

Uses
  SynHighlighterMulti, DGHSynEdit;

Type
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
    (Description : '<Alt> key invokes Column Selection Mode'; Value: eoAltSetsColumnMode),
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

Var
  (** A private variable to give the EnumFontProc access to the ComboBox. **)
  FontNames : TComboBox;

(**

  An enumeration procedure for extracting fixed width fonts.

  @precon  None.
  @postcon Adds fixed width fonts to the combo box.

  @param   LogFont    as a PEnumLogFontEx
  @param   TextMetric as a PNewTextMetric
  @param   FontType   as an Integer
  @param   lParam     as a LPARAM
  @return  an Integer   

**)
Function FontEnumExProc(LogFont : PEnumLogFontEx; TextMetric : PNewTextMetric;
  FontType : Integer; lParam : LPARAM) : Integer; StdCall;

Var
  S : String;

Begin
  With TEnumLogFontEx(LogFont^) Do
    If elfLogFont.lfPitchAndFamily And FIXED_PITCH > 0 Then
      Begin
        S := elfLogFont.lfFaceName;
        If FontNames.Items.IndexOf(S) = -1 Then
          FontNames.Items.AddObject(S, TObject(FontType));
      End;
  Result := 1;
End;

(**

  This is the constructor method for the TAttribute class.

  @precon  None.
  @postcon Creates an instance of a TAttribute class.

  @param   Name   as a String
  @param   Fore   as a TColor
  @param   Back   as a TColor
  @param   Style  as a TFontStyles
  @param   Attr   as a TSynHighlighterAttributes
  @param   Parent as a TSynCustomHighlighter

**)
Constructor TAttribute.Create(Name : String; Fore, Back : TColor;
  Style : TFontStyles; Attr : TSynHighlighterAttributes;
  Parent : TSynCustomHighlighter);

Begin
  FName       := Name;
  FForeColour := Fore;
  FBackColour := Back;
  FStyle      := Style;
  FAttribute  := Attr;
  FParent     := Parent;
End;

(**

  This method adds a TAttrbute to the collection based on the passed
  TSynHighlighterAttributes.

  @precon  Attr and Parent must be valid instanes of an attribute and
           highlighter respectively.
  @postcon Adds a TAttrbute to the collection based on the passed
           TSynHighlighterAttributes.

  @param   Attr   as a TSynHighlighterAttributes
  @param   Parent as a TSynCustomHighlighter
  @return  a TAttribute

**)
Function TAttributes.Add(Attr : TSynHighlighterAttributes;
  Parent : TSynCustomHighlighter) : TAttribute;

Begin
  Result := Nil;
  If FProcessedAttrs.IndexOf(Attr) = -1 Then  // Only add it not already in list
    Begin
      FProcessedAttrs.Add(Attr);
      Result := TAttribute.Create(Attr.Name, Attr.Foreground, Attr.Background,
        Attr.Style, Attr, Parent);
      FAttributes.Add(Result);
    End;
End;

(**

  This is a constructor method for the TAttributes class.
  
  @precon  None.
  @postcon Initialise the collection.
  
**)
Constructor TAttributes.Create;

Begin
  FAttributes := TObjectList.Create(True);
  FProcessedAttrs := TList.Create;
End;

(**

  This is a destructor for the Attributes class.
  
  @precon  None.
  @postcon Frees the classes internal memory.
  
**)
Destructor TAttributes.Destroy;

Begin
  FProcessedAttrs.Free;
  FAttributes.Free;
End;

(**

  This method updates the referenced highlighter attributes and highlighter.
  
  @precon  None.
  @postcon Updates the referenced highlighter attributes and highlighter.

  @param   boolIncTags as a Boolean

**)
Procedure TAttributes.Update(boolIncTags : Boolean);

Var
  i : Integer;

Begin
  For i := 0 To FAttributes.Count - 1 Do
    With FAttributes[i] As TAttribute Do
      Begin
        Attribute.Foreground := ForeColour;
        Attribute.Background := BackColour;
        Attribute.Style      := Style;
        If boolIncTags Then
          Parent.Tag := Parent.Tag + 1;
      End;
End;

(**

  This method add the attributes of the given highlighters to the attribute
  collection.
  
  @precon  Highlighter must be a valid instance.
  @postcon Add the attributes of the given highlighters to the attribute
           collection.
           
  @param   Highlighter as a TSynCustomHighlighter

**)
Procedure TfrmEditorOptions.AddHighlighter(Highlighter : TSynCustomHighlighter);

Var
  A : TAttribute;
  strName : String;
  i : Integer;

Begin
  strName := HighlighterName(Highlighter);
  For i := 0 To Highlighter.AttrCount - 1 Do
    Begin
      A := FAttributes.Add(Highlighter.Attribute[i], Highlighter);
      If A <> Nil Then
        lbAttributes.Items.AddObject(Format('%s:%s', [strName,
          Highlighter.Attribute[i].Name]), A);
    End;
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
  j : TSynEditorOption;
  FontInfo : TLogFont;

Begin
  Inherited Create(AOwner);
  FAttributes := TAttributes.Create;
  FontInfo.lfCharSet := DEFAULT_CHARSET;
  FontInfo.lfFaceName := '';
  FontInfo.lfPitchAndFamily := FIXED_PITCH;
  FontNames := cbxFontName;
  EnumFontFamiliesEx(Canvas.Handle, FontInfo, @FontEnumExProc, 0,
    Integer(cbxFontName));
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

  @param   OwnerForm  as a TForm
  @param   Editor     as a TSynEdit
  @param   boolIncTag as a Boolean
  @return  a Boolean

**)
Class Function TfrmEditorOptions.Execute(OwnerForm : TForm; Editor : TSynEdit;
  boolIncTag : Boolean) : Boolean;

Var
  i : TSynEditorOption;
  M : TSynMultiSyn;
  S : TScheme;
  j : Integer;
  A : TAttribute;

Begin
  Result := False;
  With TfrmEditorOptions.Create(OwnerForm) Do
    Try
      // Visual
      udTabWidth.Position := Editor.TabWidth;
      cbxEditorBackgroundColour.Selected := Editor.Color;
      cbxActiveLineColour.Selected := Editor.ActiveLineColor;
      cbxFontName.ItemIndex := cbxFontName.Items.IndexOf(Editor.Font.Name);
      udEditorFontSize.Position := Editor.Font.Size;
      chxLineNumbers.Checked := Editor.Gutter.ShowLineNumbers;
      chkWordWrap.Checked := Editor.WordWrap;
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
          If Editor.Highlighter Is TSynMultiSyn Then
            Begin
              M := Editor.Highlighter As TSynMultiSyn;
              AddHighlighter(M.DefaultHighlighter);
              lblHighlighterType.Caption := HighlighterName(M.DefaultHighlighter);
              For j := 0 To M.Schemes.Count - 1 Do
                Begin
                  S := M.Schemes[j] As TScheme;
                  A := FAttributes.Add(S.MarkerAttri, M);
                  If A <> Nil Then
                    lbAttributes.Items.AddObject(Format('%s:%s', [
                      S.SchemeName, 'Marker']), A);
                  AddHighlighter(S.Highlighter);
                End;
            End Else
            Begin
              lblHighlighterType.Caption := HighlighterName(Editor.Highlighter);
              AddHighlighter(Editor.Highlighter);
            End;
        End Else
          SyntaxTab.TabVisible := False;
      // Initialise the Highlighter Attributes.
      If lbAttributes.Items.Count > 0 Then
        Begin
          lbAttributes.ItemIndex := 0;
          lbAttributesClick(Nil);
        End;
      PageControl1.ActivePageIndex := 0;
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
          Editor.WordWrap := chkWordWrap.Checked;
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
          FAttributes.Update(boolIncTag);
          Result := True;
        End;
    Finally
      Free;
    End;
End;

(**

  This is the attribute list box`s on click event handler.

  @precon  None.
  @postcon Updates the attribute conotrols with the selected attribute in the
           list box.

  @param   Sender as a TObject

**)
procedure TfrmEditorOptions.lbAttributesClick(Sender: TObject);

Var
  A : TAttribute;

begin
  If lbAttributes.ItemIndex > -1 Then
    Begin
      A := lbAttributes.Items.Objects[lbAttributes.ItemIndex] As TAttribute;
      If A <> Nil Then
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
  A : TAttribute;

begin
  If lbAttributes.ItemIndex > -1 Then
    If Not FUpdating Then
      Begin
        A := lbAttributes.Items.Objects[lbAttributes.ItemIndex] As TAttribute;
        If A <> Nil Then
          Begin
            A.ForeColour := cbxAttrForeColour.Selected;
            A.BackColour := cbxAttrBackColour.Selected;
            A.Style := [];
            If cbxBold.Checked Then
              A.Style := A.Style + [fsBold];
            If cbxItalic.Checked Then
              A.Style := A.Style + [fsItalic];
            If cbxUnderlined.Checked Then
              A.Style := A.Style + [fsUnderline];
            If cbxStrikeout.Checked Then
              A.Style := A.Style + [fsStrikeout];
          End;
      End;
end;

end.
