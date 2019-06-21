(**

  This module contains useful functions for working with SynEdit code.

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
unit UsefulSynEditFunctions;

interface

Uses
  IniFiles, SynEditHighlighter, SynHighlighterMulti;

  procedure LoadHighlighterFromINIFile(iniFile : TMemIniFile; Highlighter: TSynCustomHighlighter);
  procedure SaveHighlighterToINIFile(iniFile : TMemINIFile; Highlighter: TSynCustomHighlighter);

implementation

Uses
  Graphics, SysUtils, DGHSynEdit;

(**

  This method loads the given highlighter information from the given ini file.

  @precon  INIFile and Highlighter must be valid instances.
  @postcon Loads the given highlighter information from the given ini file.

  @param   iniFile     as a TMemIniFile
  @param   Highlighter as a TSynCustomHighlighter

**)
procedure LoadHighlighterFromINIFile(iniFile : TMemIniFile; Highlighter: TSynCustomHighlighter);

Var
  iAttr : Integer;
  A : TSynHighlighterAttributes;
  strKey : String;
  M : TSynMultiSyn;
  S : TScheme;
  strName : String;
  iScheme : Integer;

begin
  With IniFile Do
    Begin
      If Highlighter Is TSynMultiSyn Then
        Begin
          M := Highlighter As TSynMultiSyn;
          If M.DefaultHighlighter.Tag < 0 Then
            LoadHighlighterFromINIFile(iniFile, M.DefaultHighlighter);
          For iScheme := 0 To M.Schemes.Count - 1 Do
            Begin
              S := M.Schemes[iScheme] As TScheme;
              A := S.MarkerAttri;
              strKey := HighlighterName(M);
              strName := Format('%s:%s', [S.SchemeName, A.Name]);
              A.Background := StringToColor(ReadString(strKey, strName + '.Background',
                ColorToString(A.Background)));
              A.Foreground := StringToColor(ReadString(strKey, strName + '.Foreground',
                ColorToString(A.Foreground)));
              A.Style := TFontStyles(Byte(ReadInteger(strKey, strName + '.Style',
                Byte(A.Style))));
              If Highlighter.Tag < 0 Then
                LoadHighlighterFromINIFile(iniFile, S.Highlighter);
            End;
        End Else
          For iAttr := 0 To Highlighter.AttrCount - 1 Do
            Begin
              A := Highlighter.Attribute[iAttr];
              strKey := HighlighterName(Highlighter);
              A.Background := StringToColor(ReadString(strKey, A.Name + '.Background',
                ColorToString(A.Background)));
              A.Foreground := StringToColor(ReadString(strKey, A.Name + '.Foreground',
                ColorToString(A.Foreground)));
              A.Style := TFontStyles(Byte(ReadInteger(strKey, A.Name + '.Style',
                Byte(A.Style))));
            End;
      Highlighter.Tag := 0;
    End;
end;

(**

  This method saves the given highlighter to the given ini file.

  @precon  INIFile and Highlighter must be valid instances.
  @postcon Saves the given highlighter to the given ini file.

  @param   INIFile     as a TMemINIFile
  @param   Highlighter as a TSynCustomHighlighter

**)
procedure SaveHighlighterToINIFile(INIFile : TMemINIFile;
  Highlighter: TSynCustomHighlighter);

Var
  iAttr : Integer;
  A : TSynHighlighterAttributes;
  strKey : String;
  S : TScheme;
  M : TSynMultiSyn;
  iScheme : Integer;
  strName : String;

begin
  With IniFile Do
    Begin
      If Highlighter Is TSynMultiSyn Then
        Begin
          M := Highlighter As TSynMultiSyn;
          For iScheme := 0 To M.Schemes.Count - 1 Do
            Begin
              S := M.Schemes[iScheme] As TScheme;
              A := S.MarkerAttri;
              strKey := HighlighterName(M);
              strName := Format('%s:%s', [S.SchemeName, A.Name]);
              WriteString(strKey, strName + '.Background', ColorToString(A.Background));
              WriteString(strKey, strName + '.Foreground', ColorToString(A.Foreground));
              WriteInteger(strKey, strName + '.Style', Byte(A.Style));
            End;
        End Else
        For iAttr := 0 To Highlighter.AttrCount - 1 Do
          Begin
            A := Highlighter.Attribute[iAttr];
            strKey := HighlighterName(Highlighter);
            WriteString(strKey, A.Name + '.Background', ColorToString(A.Background));
            WriteString(strKey, A.Name + '.Foreground', ColorToString(A.Foreground));
            WriteInteger(strKey, A.Name + '.Style', Byte(A.Style));
          End;
      UpdateFile;
    End;
end;

end.
