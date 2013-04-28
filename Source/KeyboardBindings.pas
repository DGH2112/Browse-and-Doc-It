(**

  This module contains the keyboard bindings that the Browse And Doc It
  module installs into the IDE.

  @Version 1.0
  @Author  David Hoyle
  @Date    28 Apr 2013
  
**)
Unit KeyboardBindings;

Interface

{$INCLUDE ..\..\..\Library\CompilerDefinitions.inc}

Uses
  ToolsAPI,
  BrowseAndDocItWizard,
  Classes;

Type
  (** This class represents a key binding notifier for installing and handling
      key bindings for this plugin. **)
  TKeyboardBinding = Class(TNotifierObject, IOTANotifier, IOTAKeyboardBinding)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FWizard: TBrowseAndDocItWizard;
    Procedure FocusModuleExplorer(Const Context: IOTAKeyContext; KeyCode: TShortcut;
      Var BindingResult: TKeyBindingResult);
    //Procedure ShowTokens(const Context: IOTAKeyContext;
    //  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function GetBindingType: TBindingType;
    Function GetDisplayName: String;
    Function GetName: String;
    Procedure BindKeyboard(Const BindingServices: IOTAKeyBindingServices);
    Constructor Create(Wizard: TBrowseAndDocItWizard);
  End;

Implementation

Uses
  Menus,
  BaseLanguageModule,
  ToolsAPIUtils,
  TokenForm;

(**

  This method binds all the Browse and Doc It keybindings to the methods in this
  class.

  @precon  None.
  @postcon All the keybinding are bound.

  @param   BindingServices as an IOTAKeyBindingServices as a constant

**)
Procedure TKeyboardBinding.BindKeyboard(Const BindingServices: IOTAKeyBindingServices);
Begin
  BindingServices.AddKeyBinding([Shortcut(13, [ssCtrl, ssShift, ssAlt])],
    FocusModuleExplorer, Nil);
  // BindingServices.AddKeyBinding([Shortcut(Ord('T'), [ssCtrl, ssShift, ssAlt])], ShowTokens, Nil);
End;

(**

  This is a keyboard binding event hanlder for showing the tokens in the parser.

  @precon  None.
  @postcon Displays a form containsing the tokens in the current editor.

  @param   Context       as an IOTAKeyContext as a constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference


procedure TKeyboardBinding.ShowTokens(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);

Var
  SourceEditor : IOTASourceEditor;
  Source : TBaseLanguageModule;

begin
  SourceEditor := ActiveSourceEditor;
  If SourceEditor = Nil Then
    Exit;
  Source := Dispatcher(EditorAsString(SourceEditor), SourceEditor.FileName,
    SourceEditor.Modified, []);
  If Source <> Nil Then
    Try
      TfrmTokenForm.Execute(Source);
    Finally
      Source.Free;
    End;
  BindingResult := krHandled;
end; **)

(**

  This is the constructor method for the TKeyboardBinding class.

  @precon  None.
  @postcon Initialises the internal wizard reference.

  @param   Wizard as a TBrowseAndDocItWizard

**)
Constructor TKeyboardBinding.Create(Wizard: TBrowseAndDocItWizard);
Begin
  FWizard := Wizard;
End;

(**

  This method is a handler for the Focus Modul Explorer keyboatf binding.

  @precon  None.
  @postcon Asks the main wizard for focus the module explorer.

  @param   Context       as an IOTAKeyContext as a constant
  @param   KeyCode       as a TShortcut
  @param   BindingResult as a TKeyBindingResult as a reference

**)
Procedure TKeyboardBinding.FocusModuleExplorer(Const Context: IOTAKeyContext;
  KeyCode: TShortcut; Var BindingResult: TKeyBindingResult);
Begin
  FWizard.ModuleExplorerClick(Self);
  BindingResult := krHandled;
End;

(**

  This is a getter method for the BindingType property.

  @precon  None.
  @postcon Defines the keyboard binding as a partial binding set.

  @return  a TBindingType

**)
Function TKeyboardBinding.GetBindingType: TBindingType;
Begin
  Result := btPartial;
End;

(**

  This is a getter method for the DisplayName property.

  @precon  None.
  @postcon Returns the diosplay name for the set of Keyboard Bindings.

  @return  a string

**)
Function TKeyboardBinding.GetDisplayName: String;
Begin
  Result := 'Browse And Doc It Comment Bindings';
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of th keyboard binding set.

  @return  a string

**)
Function TKeyboardBinding.GetName: String;
Begin
  Result := 'BrowseAndDocItBindings';
End;

End.
