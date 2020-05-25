(**
  
  This module contains a class whicih imnplements the IOTAEditorNotifier for detecting then views are
  opened and closed.

  @Author  David Hoyle
  @Version 1.572
  @Date    23 May 2020
  
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
Unit BADI.SourceEditorNotifier;

Interface

Uses
  ToolsAPI,
  System.Classes;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class implements an IOTAEditorNotifier to tracker when views are created and destroyed. **)
  TBADISourceEditorNotifier = Class(TNotifierObject, IInterface, IOTANotifier, IOTAEditorNotifier)
  Strict Private
    {$IFDEF DXE100}
    FEditViewNotifierIndex : Integer;
    {$ENDIF DXE100}
    FView                  : IOTAEditView;
    FFilenames             : TStringList;
  Strict Protected
    Procedure ViewActivated(Const View: IOTAEditView);
    Procedure ViewNotification(Const View: IOTAEditView; Operation: TOperation);
  Public
    Constructor Create(Const SE : IOTASourceEditor);
    Destructor Destroy; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  System.SysUtils,
  System.TypInfo,
  BADI.EditViewNotifier;

(**

  A constructor for the TBADISourceEditorNotifier class.

  @precon  None.
  @postcon Initialises the class and creates a view if a edit view is available. This is a workaround
           for new modules created afrer the IDE has started.

  @param   SE as an IOTASourceEditor as a constant

**)
Constructor TBADISourceEditorNotifier.Create(Const SE : IOTASourceEditor);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  {$IFDEF DXE100}
  FEditViewNotifierIndex := -1;
  {$ENDIF DXE100}
  FView := Nil;
  FFilenames := TStringList.Create;
  FFilenames.Sorted := True;
  // Workaround for new modules create after the IDE has started
  If SE.EditViewCount > 0 Then
    ViewNotification(SE.EditViews[0], opInsert);
End;

(**

  A destructor for the TBADISourceEditorNotifier class.

  @precon  None.
  @postcon Tries to remove the view notifier.

**)
Destructor TBADISourceEditorNotifier.Destroy;

ResourceString
  strFilenameOrphaned = 'TBADISourceEditorNotifier.Destroy, Filename "%s" orphaned!';

Var
  strFileName : String;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  ViewNotification(FView, opRemove);
  For strFileName In FFilenames Do
    CodeSite.SendFmtMsg(csmError, strFilenameOrphaned, [strFileName]);
  FFilenames.Free;
  Inherited Destroy;
End;

(**

  This method is called when an view is activated.

  @precon  None.
  @postcon Not used.

  @nocheck EmptyMethod
  @nohint  View

  @param   View as an IOTAEditView as a constant

**)
Procedure TBADISourceEditorNotifier.ViewActivated(Const View: IOTAEditView);

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ViewActivated', tmoTiming);{$ENDIF}
End;

(**

  This method is called when a view is created and when it is destroyer.

  @precon  None.
  @postcon This method either installs or removes an Edit View notifier for paining on the editor.

  @nocheck MissingCONSTInParam

  @param   View      as an IOTAEditView as a constant
  @param   Operation as a TOperation

**)
Procedure TBADISourceEditorNotifier.ViewNotification(Const View: IOTAEditView; Operation: TOperation);
var
  iIndex: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ViewNotification', tmoTiming);{$ENDIF}
  {$IFDEF DXE100}
  If Assigned(View) Then
    Begin
      Case Operation Of
        // Only create a notifier if one has not already been created!
        opInsert:
          If FEditViewNotifierIndex = -1 Then 
            Begin
              If Not FFilenames.Find(View.Buffer.FileName, iIndex) Then
                Begin
                  FFilenames.Add(View.Buffer.FileName);
                  {$IFDEF CODESITE}
                  CodeSite.Send(
                    csmReminder,
                    'TBADISourceEditorNotifier.ViewNotification.Added',
                    ExtractFileName(View.Buffer.FileName)
                  );
                  {$ENDIF CODESITE}
                End {$IFDEF CODESITE} Else
                  CodeSite.Send(
                    csmWarning,
                    'TBADISourceEditorNotifier.ViewNotification.Exists',
                    ExtractFileName(View.Buffer.FileName)
                  ) {$ENDIF CODESITE};
              FView := View;
              FEditViewNotifierIndex := View.AddNotifier(TBADIEditViewNotifier.Create);
            End;
        // opRemove Never gets called!
        opRemove:
          If FEditViewNotifierIndex > -1 Then
            Begin
              If FFilenames.Find(View.Buffer.FileName, iIndex) Then
                Begin
                  FFilenames.Delete(iIndex);
                  {$IFDEF CODESITE}
                  CodeSite.Send(
                    csmReminder,
                    'TBADISourceEditorNotifier.ViewNotification.Removed',
                    ExtractFileName(View.Buffer.FileName)
                  );
                  {$ENDIF CODESITE}
                End {$IFDEF CODESITE} Else
                  CodeSite.Send(
                    csmWarning,
                    'TBADISourceEditorNotifier.ViewNotification.Not found',
                    ExtractFileName(View.Buffer.FileName)
                  ) {$ENDIF CODESITE};
              View.RemoveNotifier(FEditViewNotifierIndex);
              FEditViewNotifierIndex := -1;
            End;
      End;
    End;
  {$ENDIF DXE100}
End;

End.



