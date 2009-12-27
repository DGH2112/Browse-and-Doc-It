(**

  This module contains useful functions for working with SynEdit code.

  @Version 1.0
  @Date    27 Dec 2009
  @Author  David Hoyle

**)
unit UsefulSynEditFunctions;

interface

Uses
  IniFiles, SynEditHighlighter, SynHighlighterMulti;

  procedure LoadHighlighterFromINIFile(FINIFile : String; Highlighter: TSynCustomHighlighter);
  procedure SaveHighlighterToINIFile(INIFile : TINIFile; Highlighter: TSynCustomHighlighter);

implementation

Uses
  Graphics, SysUtils, DGHSynEdit;

(**

  This method loads the given highlighter information from the given ini file.

  @precon  INIFile and Highlighter must be valid instances.
  @postcon Loads the given highlighter information from the given ini file.

  @param   FINIFile    as a String
  @param   Highlighter as a TSynCustomHighlighter

**)
procedure LoadHighlighterFromINIFile(FINIFile : String; Highlighter: TSynCustomHighlighter);

Var
  iAttr : Integer;
  A : TSynHighlighterAttributes;
  strKey : String;
  M : TSynMultiSyn;
  S : TScheme;
  strName : String;
  iScheme : Integer;

begin
  With TIniFile.Create(FINIFile) Do
    Try
      If Highlighter Is TSynMultiSyn Then
        Begin
          M := Highlighter As TSynMultiSyn;
          If M.DefaultHighlighter.Tag < 0 Then
            LoadHighlighterFromINIFile(FINIFile, M.DefaultHighlighter);
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
                LoadHighlighterFromINIFile(FINIFile, S.Highlighter);
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
    Finally
      Free;
    End;
end;

(**

  This method saves the given highlighter to the given ini file.

  @precon  INIFile and Highlighter must be valid instances.
  @postcon Saves the given highlighter to the given ini file.

  @param   INIFile     as a TINIFile
  @param   Highlighter as a TSynCustomHighlighter

**)
procedure SaveHighlighterToINIFile(INIFile : TINIFile;
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
end;

end.
