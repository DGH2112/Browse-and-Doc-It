(**
  
  A module to provide an custom editor sub view for displaying the metrics for a module.

  @Author  David Hoyle
  @Version 1.016
  @Date    24 Feb 2024

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
Unit BADI.Module.Metrics.SubView;

Interface

Uses
  ToolsAPI,
  StdActns,
  DesignIntf,
  Forms,
  Generics.Collections;

Type
  (** A class to implement the INTACustomEditorSubView interface for a sub-view for metrics. **)
  TBADIModuleMetricsSubView = Class(TInterfacedObject, IInterface, INTACustomEditorSubView)
  Strict Private
  Strict Protected
    // INTACustomEditorSubView
    Procedure Display(Const AContext: IInterface; AViewObject: TObject);
    Function  EditAction(Const AContext: IInterface; Action: TEditAction; AViewObject: TObject): Boolean;
    Procedure FrameCreated(AFrame: TCustomFrame);
    Function  GetCanCloneView: Boolean;
    Function  GetCaption: String;
    Function  GetEditState(Const AContext: IInterface; AViewObject: TObject): TEditState;
    Function  GetFrameClass: TCustomFrameClass;
    Function  GetPriority: Integer;
    Function  GetViewIdentifier: String;
    Function  Handles(Const AContext: IInterface): Boolean;
    Procedure Hide(Const AContext: IInterface; AViewObject: TObject);
    Procedure ViewClosed(Const AContext: IInterface; AViewObject: TObject);
    // General Methods
  Public
    Class Function CreateEditorMetricsSubView : INTACustomEditorSubView;
    Constructor Create;
    Destructor Destroy; Override;
  End;

  Procedure RegisterEditorMetricsSubView;
  Procedure UnregisterEditorMetricsSubView;

Implementation

Uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  Controls, 
  BADI.ToolsAPIUtils, 
  BADI.Module.Dispatcher, 
  BADI.Base.Module, 
  BADI.Types, 
  BADI.Module.Metrics.SubView.Frame,
  BADI.Module.Metrics.EditorView.Frame;

Var
  (** A private pointer to the registered sub-view so it can be removed. **)
  ptrEditorMetricsSubView : Pointer;

(**

  This method registers the custom editor sub-view with the IDE.

  @precon  None.
  @postcon The sub-view is registered with the IDE.

**)
Procedure RegisterEditorMetricsSubView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    ptrEditorMetricsSubView := EVS.RegisterEditorSubView(
      TBADIModuleMetricsSubView.CreateEditorMetricsSubView);
End;

(**

  This method un-registers the custom editor sub-view from the IDE.

  @precon  None.
  @postcon The sub-view is un-registered from the IDE.

**)
Procedure UnregisterEditorMetricsSubView;

Var
  EVS : IOTAEditorViewServices;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    EVS.UnregisterEditorSubView(ptrEditorMetricsSubView);
End;

{ TBADIModuleMetricsSubView }

(**

  A constructor for the TBADIModuleMetricsSubView class.

  @precon  None.
  @postcon Does nothing.

  @nocheck EmptyMethod

**)
Constructor TBADIModuleMetricsSubView.Create;

Begin
End;

(**

  This is a class function to create the custom editor sub-view for displaying module metric information.

  @precon  None.
  @postcon Returns a new instance of the sub-view.

  @return  an INTACustomEditorSubView

**)
Class Function TBADIModuleMetricsSubView.CreateEditorMetricsSubView: INTACustomEditorSubView;

Begin
  Result := TBADIModuleMetricsSubView.Create;
End;

(**

  A destructor for the TBADIModuleMetricsSubView class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TBADIModuleMetricsSubView.Destroy;

Begin
  Inherited Destroy;
End;

(**

  This method is called when a sub-view is displayed or hidden (AContext is nil).

  @precon  None.
  @postcon If the context is valid the modules editor text is parsed and passed to the sub-view for
           rendering.

  @nocheck MissingCONSTInParam

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject

**)
Procedure TBADIModuleMetricsSubView.Display(Const AContext: IInterface; AViewObject: TObject);

Var
  EVS : IOTAEditorViewServices;
  OTAModule : IOTAModule;
  Module : TBaseLanguageModule;
  strSource: String;
  SE : IOTASourceEditor;
  
Begin
  If Supports(BorlandIDEServices, IOTAEditorViewServices, EVS) Then
    If Assigned(AContext) Then
      If EVS.ContextToModule(AContext, OTAModule) Then
        Begin
          SE := TBADIToolsAPIFunctions.SourceEditor(OTAModule);
          strSource := TBADIToolsAPIFunctions.EditorAsString(SE);
          Module := TBADIDispatcher.BADIDispatcher.Dispatcher(strSource, SE.FileName, SE.Modified,
            [moParse]);
          Try
            (AViewObject As TframeBADIModuleMetricsSubView).RenderModule(
              Module,
              [roClear, roAutoExpand]
            );
          Finally
            Module.Free;
          End;
        End;
End;

(**

  This method is called by the IDE to undertake an editor actions the sub-view supports.

  @precon  None.
  @postcon If copy is the action the sub-view asks the frame to copy the metric information to the
           clipboard.

  @nocheck MissingCONSTInParam
  @nohint  AContext

  @param   AContext    as an IInterface as a constant
  @param   Action      as a TEditAction
  @param   AViewObject as a TObject
  @return  a Boolean

**)
Function TBADIModuleMetricsSubView.EditAction(Const AContext: IInterface; Action: TEditAction;
  AViewObject: TObject): Boolean;

Begin
  Result := False;
  Case Action Of
    eaCopy:
      Begin
        (AViewObject As TframeBADIModuleMetricsSubView).CopyToClipboard;
        REsult := True;
      End;
  End;
End;

(**

  This method is called by the IDE when it creates the frame instance.

  @precon  None.
  @postcon Store a reference to the frame instance so it can be used later in Display.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  AFrame

  @param   AFrame as a TCustomFrame

**)
Procedure TBADIModuleMetricsSubView.FrameCreated(AFrame: TCustomFrame);

Begin
End;

(**

  This is a getter method for the CanCloseView property.

  @precon  None.
  @postcon Called by the IDE to know whether the view can be closed. We return false to let the IDE know
           if cannot be cloned.

  @return  a Boolean

**)
Function TBADIModuleMetricsSubView.GetCanCloneView: Boolean;

Begin
  Result := True;
End;

(**

  This is a getter method for the Caption property.

  @precon  None.
  @postcon Called by the IDE to get the sub-view tab caption.

  @return  a String

**)
Function TBADIModuleMetricsSubView.GetCaption: String;

ResourceString
  strMetrics = 'Metrics';

Begin
  Result := strMetrics;
End;

(**

  This is a getter method for the Edit State property.

  @precon  None.
  @postcon Called by the IDE to find out which editor actions can be undertaken by the sub-view. We return
           a set with esCanCopy to signify that the sub-view can copy the metric data to the clipboard.

  @nocheck MissingCONSTInParam
  @nohint  AContext AViewObject

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject
  @return  a TEditState

**)
Function TBADIModuleMetricsSubView.GetEditState(Const AContext: IInterface;
  AViewObject: TObject): TEditState;

Begin
  Result := [esCanCopy];
End;

(**

  This is a getter method for the Frame Class property.

  @precon  None.
  @postcon This method is called by the IDE to get the class of the frame to be created by the IDE for
           the sub-view. We returns the class of our custom frame for viewing metrics.

  @return  a TCustomFrameClass

**)
Function TBADIModuleMetricsSubView.GetFrameClass: TCustomFrameClass;

Begin
  Result := TframeBADIModuleMetricsSubView;
End;

(**

  This is a getter method for the Priority property.

  @precon  None.
  @postcon This method is called by the IDE to find out the priority of the sub-view caption tab. Our
           metrics tab is low priority so that it is on the right of all other tabs.

  @return  an Integer

**)
Function TBADIModuleMetricsSubView.GetPriority: Integer;

Begin
  Result := svpLow;
End;

(**

  This is a getter method for the View Identifier property.

  @precon  None.
  @postcon Returns a unique name for the sub-view.

  @return  a String

**)
Function TBADIModuleMetricsSubView.GetViewIdentifier: String;

Const
  strBADIMetricsSubView = 'BADICustomEditorMetricsSubView';

Begin
  Result := strBADIMetricsSubView;
End;

(**

  This method is called by the IDE to understand which contexts are handled by the sub-view.

  @precon  None.
  @postcon We need to IOTAModule context for our sub-view so we return true for that context only.

  @param   AContext as an IInterface as a constant
  @return  a Boolean

**)
Function TBADIModuleMetricsSubView.Handles(Const AContext: IInterface): Boolean;

Var
  EVS : IOTAEditorViewServices;
  Module: IOTAModule;
  
Begin
  Result := False;
  If Assigned(AContext) Then
    If Supports(BorlandIDEServices, IOTAEditorVIewServices, EVS) Then
      Result := EVS.ContextToModule(AContext, Module);
End;

(**

  This method is called when a sub-view is hidden by another sub-view.

  @precon  None.
  @postcon Not used.

  @nocheck EmptyMethod MissingCONSTInParam
  @nohint  AContext AViewObject

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject

**)
Procedure TBADIModuleMetricsSubView.Hide(Const AContext: IInterface; AViewObject: TObject);

Begin
  // Do nothing
End;

(**

  This method is called then the IDE closes down and closes all the sub-views all at once.

  @precon  None.
  @postcon Not used.

  @nocheck MissingCONSTInParam EmptyMethod
  @nohint  AContext AViewObject

  @param   AContext    as an IInterface as a constant
  @param   AViewObject as a TObject

**)
Procedure TBADIModuleMetricsSubView.ViewClosed(Const AContext: IInterface; AViewObject: TObject);

Begin
  // Do nothing
End;

End.
