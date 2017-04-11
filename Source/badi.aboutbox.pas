(**

  This module contains code for creating the about box entry in RAD Studio.

  @Author  David Hoyle
  @Version 1.0
  @Date    11 Apr 2017

**)
Unit BADI.AboutBox;

Interface

{$INCLUDE CompilerDefinitions.inc}

  Procedure AddAboutBoxEntry;
  Procedure RemoveAboutBoxEntry;

Implementation

Uses
  ToolsAPI,
  SysUtils,
  Windows,
  BADI.Functions,
  Forms,
  BADI.Constants;

{$IFDEF D2005}
Var
  (** An index for the About Box Plugin. - required for unloading the interface. **)
  iAboutPlugin : Integer;
{$ENDIF}

(**

  This procedure adds the about box entry to the RAD Studio IDE.

  @precon  None.
  @postcon If the version of RAD Studio is equal to or above 2005 the about box entry is
           added to the IDE.

**)
Procedure AddAboutBoxEntry;

Var
  iMajor : Integer;
  iMinor : Integer;
  iBugFix : Integer;
  iBuild : Integer;
  bmSplashScreen : HBITMAP;

Begin
  {$IFDEF D2005}
  BuildNumber(iMajor, iMinor, iBugFix, iBuild);
  bmSplashScreen := LoadBitmap(hInstance, 'BrowseAndDocItSplashScreenBitMap48x48');
  iAboutPlugin := (BorlandIDEServices As IOTAAboutBoxServices).AddPluginInfo(
    Format(strSplashScreenName, [iMajor, iMinor, Copy(strRevision, iBugFix + 1, 1),
      Application.Title]),
    'An IDE expert to browse and document your source code.',
    bmSplashScreen,
    False,
    Format(strSplashScreenBuild, [iMajor, iMinor, iBugfix, iBuild]),
    Format('SKU Build %d.%d.%d.%d', [iMajor, iMinor, iBugfix, iBuild]));
  {$ENDIF}
End;

(**

  This procedure removes the about box entry from the IDE.

  @precon  None.
  @postcon The about box entry is removed from the IDE.

**)
Procedure RemoveAboutBoxEntry;

Begin
  {$IFDEF D2010}
  If iAboutPlugin > iWizardFailState Then
    (BorlandIDEServices As IOTAAboutBoxServices).RemovePluginInfo(iAboutPlugin);
  {$ENDIF}
End;

End.
