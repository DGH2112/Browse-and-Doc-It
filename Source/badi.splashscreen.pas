(**

  This module contains code to add an entry to the RAD Studio splash screen.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Feb 2017

**)
Unit BADI.SplashScreen;

Interface

{$INCLUDE CompilerDefinitions.inc}

  Procedure AddSplashScreen;

Implementation

Uses
  ToolsAPI,
  SysUtils,
  Windows,
  Forms,
  BADI.Common;

(**

  This method adds an entry to the RAD Studio IDE splash screen.

  @precon  None.
  @postcon An entry is added to the RAD Studio IDE slplash screen if the version of RAD
           Studio is 2005 and above.

**)
Procedure AddSplashScreen;

Var
  iMajor : Integer;
  iMinor : Integer;
  iBugFix : Integer;
  iBuild : Integer;
  bmSplashScreen : HBITMAP;

Begin
  {$IFDEF D2005}
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  {$IFDEF D2007}
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap24x24');
  {$ELSE}
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap48x48');
  {$ENDIF}
  (SplashScreenServices As IOTASplashScreenServices).AddPluginBitmap(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1),
      Application.Title]),
    bmSplashScreen,
    False,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]), ''
    );
  {$ENDIF}
End;

End.
