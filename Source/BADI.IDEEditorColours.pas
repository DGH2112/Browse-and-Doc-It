(**
  
  This module contains a class which implements the IBADIIDEEditorColours interface to extract
  the token colours from the IDE.

  @Author  David Hoyle
  @Version 1.017
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.IDEEditorColours;

Interface

Uses
  System.Win.Registry,
  VCL.Graphics,
  BADI.Types,
  BADI.Interfaces;

Type
  (** A class which implements the IBADIIDEEditorColours interface for getting the current IDEs
      editor colours. **)
  TBADIIDEEditorColours = Class(TInterfacedObject, IBADIIDEEditorColours)
  Strict Private
    Function  GetIDEVersionNum(Const strBDSDir : String) : String;
    Function  GetIDERegPoint() : String;
    Procedure ReadHighlight(Const Reg : TRegIniFile; Const strSectionName : String;
      Var TokenFontInfo : TTokenFontInfo);
  Strict Protected
    Function GetIDEEditorColours(Var iBGColour : TColor) : TBADITokenFontInfoTokenSet;
  Public
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  System.SysUtils,
  BADI.Constants;

(**

  This method iterates each token type and loads into information from the registry (if found).

  @precon  None.
  @postcon The IDE Editor Colours are loaded.

  @param   iBGColour as a TColor as a reference
  @return  a TBADITokenFontInfoTokenSet

**)
Function TBADIIDEEditorColours.GetIDEEditorColours(Var iBGColour : TColor) : TBADITokenFontInfoTokenSet;

Const
  strBDSEnviroVar = 'BDS';
  strHelpRegKey = 'Software\Embarcadero\%s\%s\Editor\Highlight';
  strTokenHighlightMap : Array[Low(TBADITokenType)..High(TBADITokenType)] Of String = (
    'Illegal Char',                      // ttUnknown
    'Whitespace',                        // ttWhiteSpace
    'Reserved word',                     // ttReservedWord
    'Identifier',                        // ttIdentifier
    'Number',                            // ttNumber
    'Symbol',                            // ttSymbol
    'Whitespace',                        // ttLineEnd
    'String',                            // ttSingleLiteral
    'Character',                         // ttDoubleLiteral
    'Comment',                           // ttLineComment
    'Comment',                           // ttBlockComment
    'Tags',                              // ttHTMLStartTag
    'Tags',                              // ttHTMLEndTag
    'Reserved word',                     // ttDirective
    'Preprocessor',                      // ttCompilerDirective
    'Hot Link',                          // ttLinkTag
    'ttTreeHeader',                      // ttTreeHeader
    'Whitespace',                        // ttFileEnd
    'Whitespace',                        // ttLineContinuation
    'Tags',                              // ttCustomUserToken
    'Marked Block',                      // ttExplorerHighlight
    'Plain text',                        // ttPlainText
    'Comment',                           // ttCommentText
    'Attribute Names',                   // ttTagHeaderText
    'Attribute Values',                  // ttTagText 
    'Additional search match highlight', // Search Highlight
    'Line Highlight',                    // Line Highlight
    'ttDocIssueEditorText'               // Doc Issue Editor Text
  );

Var
  strBDSDir: String;
  R: TRegIniFile;
  eTokenType: TBADITokenType;

Begin
  Result := strTokenTypeInfo;
  strBDSDir := GetEnvironmentVariable(strBDSEnviroVar);
  If Length(strBDSDir) > 0 Then
    Begin
      R := TRegIniFile.Create(Format(strHelpRegKey, [GetIDERegPoint(), GetIDEVersionNum(strBDSDir)]));
      Try
        For eTokenType := Low(TBADITokenType) To High(TBADITokenType) Do
          ReadHighlight(R, strTokenHighlightMap[eTokenType], Result[eTokenType]);
        iBGColour := Result[ttPlainText].FBackColour;
      Finally
        R.Free;
      End;
    End;
End;

(**

  This method searches the IDEs command line parameters for an alternate registration point (-rXxxxx)
  and returns that alternate point instead of the standard BDS if found.

  @precon  None.
  @postcon Returns the active IDEs registration point.

  @return  a String

**)
Function TBADIIDEEditorColours.GetIDERegPoint: String;

Const
  strDefaultRegPoint = 'BDS';
  iSwitchLen = 2;

Var
  iParam: Integer;

Begin
  Result := strDefaultRegPoint;
  For iParam := 1 To ParamCount Do
    If CompareText(Copy(ParamStr(iParam), 1, iSwitchLen), '-r') = 0 Then
      Begin
        Result := ParamStr(iParam);
        System.Delete(Result, 1, iSwitchLen);
        Break;
      End;
End;

(**

  This method returns the IDEs version number from the end of the BDS environment variable passed.

  @precon  None.
  @postcon the version number is returned.

  @param   strBDSDir as a String as a constant
  @return  a String

**)
Function TBADIIDEEditorColours.GetIDEVersionNum(Const strBDSDir: String): String;

Begin
  Result := ExtractFileName(strBDSDir);
End;

(**

  This method reads an IDE Editor Token information from the given registry.

  @precon  Reg must be a valid instance.
  @postcon The token is read from the registry.

  @note    All values are stored in the registry as STRINGs.

  @param   Reg            as a TRegIniFile as a constant
  @param   strSectionName as a String as a constant
  @param   TokenFontInfo  as a TTokenFontInfo as a reference

**)
Procedure TBADIIDEEditorColours.ReadHighlight(Const Reg : TRegIniFile; Const strSectionName : String;
  Var TokenFontInfo : TTokenFontInfo);

Const
  strDefaultForeground = 'Default Foreground';
  strForegroundColorNew = 'Foreground Color New';
  strDefaultBackground = 'Default Background';
  strBackgroundColorNew = 'Background Color New';
  strBold = 'Bold';
  strItalic = 'Italic';
  strUnderline = 'Underline';
  strTrue = 'True';
  strFalse = 'False';

Begin
  // Foreground
  If CompareText(Reg.ReadString(strSectionName, strDefaultForeground, strTrue), strTrue) = 0 Then
    TokenFontInfo.FForeColour := clNone
  Else
    TokenFontInfo.FForeColour := StringToColor(Reg.ReadString(
      strSectionName,
      strForegroundColorNew,
      ColorToString(TokenFontInfo.FForeColour)));
  // Background
  If CompareText(Reg.ReadString(strSectionName, strDefaultBackground, strTrue), strTrue) = 0 Then
    TokenFontInfo.FBackColour := clNone
  Else
    TokenFontInfo.FBackColour := StringToColor(Reg.ReadString(
      strSectionName,
      strBackgroundColorNew,
      ColorToString(TokenFontInfo.FBackColour)));
  // Styles
  TokenFontInfo.FStyles := [];
  If CompareText(Reg.ReadString(strSectionName, strBold, strFalse), strTrue) = 0 Then
    Include(TokenFontInfo.FStyles, fsBold);
  If CompareText(Reg.ReadString(strSectionName, strItalic, strFalse), strTrue) = 0 Then
    Include(TokenFontInfo.FStyles, fsItalic);
  If CompareText(Reg.ReadString(strSectionName, strUnderline, strFalse), strTrue) = 0 Then
    Include(TokenFontInfo.FStyles, fsUnderline);
End;

End.
