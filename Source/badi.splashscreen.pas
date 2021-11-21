(**

  This module contains code to add an entry to the RAD Studio splash screen.

  @Author  David Hoyle
  @Version 1.098
  @Date    21 Nov 2021

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
Unit BADI.SplashScreen;

Interface

{$INCLUDE CompilerDefinitions.inc}

  Procedure AddSplashScreen;

Implementation

Uses
  ToolsAPI,
  SysUtils,
  {$IFDEF RS110}
  Graphics,
  {$ELSE}
  Windows,
  {$ENDIF}
  Forms,
  BADI.Functions,
  BADI.Constants;

(**

  This method adds an entry to the RAD Studio IDE splash screen.

  @precon  None.
  @postcon An entry is added to the RAD Studio IDE splash screen if the version of RAD
           Studio is 2005 and above.

**)
Procedure AddSplashScreen;

Const
  {$IFDEF D2007}
  strBrowseAndDocItSplashScreenBitMap = 'BrowseAndDocItSplashScreenBitMap24x24';
  {$ELSE}
  strBrowseAndDocItSplashScreenBitMap = 'BrowseAndDocItSplashScreenBitMap48x48';
  {$ENDIF}

Var
  iMajor : Integer;
  iMinor : Integer;
  iBugFix : Integer;
  iBuild : Integer;
  {$IFDEF RS110}
  SplashScreenBitMap : TBitMap;
  {$ELSE}
  bmSplashScreen : HBITMAP;
  {$ENDIF RS110}

Begin
  {$IFDEF D2005}
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  {$IFDEF RS110}
  SplashScreenBitMap := TBitMap.Create();
  Try
    SplashScreenBitMap.LoadFromResourceName(hInstance, strBrowseAndDocItSplashScreenBitMap);
    (SplashScreenServices As IOTASplashScreenServices).AddPluginBitmap(
      Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1), Application.Title]),
      [SplashScreenBitMap],
      {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
      Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
      ''
    );
  Finally
    SplashScreenBitMap.Free;
  End;
  {$ELSE}
  bmSplashScreen := LoadBitmap(hInstance, strBrowseAndDocItSplashScreenBitMap);
  (SplashScreenServices As IOTASplashScreenServices).AddPluginBitmap(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1), Application.Title]),
    bmSplashScreen,
    {$IFDEF DEBUG} True {$ELSE} False {$ENDIF},
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
    ''
  );
  {$ENDIF RS110}
  {$ENDIF D2005}
End;

End.
