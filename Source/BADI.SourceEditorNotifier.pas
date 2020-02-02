(**
  
  This module contains a class whicih imnplements the IOTAEditorNotifier for detecting then views are
  opened and closed.

  @Author  David Hoyle
  @Version 1.292
  @Date    02 Feb 2020
  
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

Type
  (** This class implements an IOTAEditorNotifier to tracker when views are created and destroyed. **)
  TBADISourceEditorNotifier = Class(TNotifierObject, IInterface, IOTANotifier, IOTAEditorNotifier)
  Strict Private
    FEditViewNotifierIndex : Integer;
    FView                  : IOTAEditView;
  Strict Protected
    Procedure ViewActivated(Const View: IOTAEditView);
    Procedure ViewNotification(Const View: IOTAEditView; Operation: TOperation);
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

Uses
  CodeSiteLogging, BADI.EditViewNotifier;

(**

  A constructor for the TBADISourceEditorNotifier class.

  @precon  None.
  @postcon Initialises the class.

**)
Constructor TBADISourceEditorNotifier.Create;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  FEditViewNotifierIndex := -1;
  FView := Nil;
End;

(**

  A destructor for the TBADISourceEditorNotifier class.

  @precon  None.
  @postcon Tries to remove the view notifier.

**)
Destructor TBADISourceEditorNotifier.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  ViewNotification(FView, opRemove);
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

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'ViewNotification', tmoTiming);{$ENDIF}
  Case Operation Of
    opInsert:
      Begin
        FView := View;
        FEditViewNotifierIndex := View.AddNotifier(TBADIEditViewNotifier.Create);
      End;
     // opRemove Never gets called!
    opRemove:
      If FEditViewNotifierIndex > -1 Then
        Begin
          View.RemoveNotifier(FEditViewNotifierIndex);
          FEditViewNotifierIndex := -1;
        End;
  End;
End;

End.


