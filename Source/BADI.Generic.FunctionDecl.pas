(**

  This module contains a class that represents a generic function (a base class for properties
  and methods).

  @Author  David Hoyle
  @Version 1.0
  @Date    05 Jan 2018

**)
Unit BADI.Generic.FunctionDecl;

Interface

Uses
  Classes,
  Contnrs,
  BADI.ElementContainer,
  BADI.Generic.TypeDecl,
  BADI.Generic.Parameter,
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class is an ancester for both methods and properties so that they
      can be handled generically (parameters and returntypes). **)
  TGenericFunction = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FParameters            : TObjectList;
    FReturnType            : TGenericTypeDecl;
    FStartLine             : Integer;
    FEndLine               : Integer;
    FStmtCount             : Integer;
    FHasProfiling          : Boolean;
    FIndent                : Integer;
    FIsDeclarationOnly     : Boolean;
    FIFStackDepth          : Integer;
    FMetrics               : Array[Low(TBADIModuleMetric)..High(TBADIModuleMetric)] Of Double;
    FChecks                : Array[Low(TBADIModuleCheck)..High(TBADIModuleCheck)] Of Double;
    FMetricOverrides       : TBADIModuleMetrics;
    FCheckOverrides        : TBADIModuleChecks;
    FToxicityPower         : Double;
    FToxicitySummartion    : TBADIToxicitySummation;
    FMetricSubOptions      : TBADIModuleMetricSubOps;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  GetQualifiedName: String; Virtual; Abstract;
    Function  GetParameterCount: Integer;
    Function  GetParameters(Const iIndex: Integer): TGenericParameter;
    Function  RequiresReturn: Boolean; Virtual; Abstract;
    Function  FunctionType: String; Virtual; Abstract;
    Function  CalculateToxicity : Double; Virtual;
    Function  VariableCount : Double; Virtual;
    Function  GetMetric(Const eMetric : TBADIModuleMetric) : Double;
    Procedure SetMetric(Const eMetric : TBADIModuleMetric; Const dblValue : Double);
    Function  GetCheck(Const eCheck : TBADIModuleCheck) : Double;
  Public
    Constructor Create(Const strName: String; Const AScope: TScope; Const iLine, iColumn: Integer;
      Const AImageIndex: TBADIImageIndex; Const AComment: TComment); Override;
    Destructor Destroy; Override;
    Procedure CheckReferences; Override;
    Function  ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    Procedure AddParameter(Const AParameter: TGenericParameter);
    Procedure IncIFDepth;
    Procedure DecIFDepth;
    Procedure IncCyclometricComplexity;
    Procedure IncrementCheck(Const eCheck : TBADIModuleCheck; Const boolOverridden : Boolean);
    (**
      This property returns the number of parameter in the parameter collection.
      @precon  None.
      @postcon Returns the number of parameter in the parameter collection.
      @return  an Integer
    **)
    Property ParameterCount: Integer Read GetParameterCount;
    (**
      This property returns an instance of the indexed parameter.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed parameter.
      @param   iIndex as an Integer as a constant
      @return  a TGenericParameter
    **)
    Property Parameters[Const iIndex: Integer]: TGenericParameter Read GetParameters;
    (**
      This property returns the type corresponding to the method return.
      @precon  None.
      @postcon Returns the type corresponding to the method return.
      @return  a TGenericTypeDecl
    **)
    Property ReturnType: TGenericTypeDecl Read FReturnType;
    (**
      Returns the Qualified name of the method.
      @precon  None.
      @postcon Returns the Qualified name of the method.
      @return  a String
    **)
    Property QualifiedName: String Read GetQualifiedName;
    (**
      This property gets and sets the start line number for the code within the
      function.
      @precon  None.
      @postcon Gets and sets the start line number for the code within the
               function.
      @return  an Integer
    **)
    Property StartLine: Integer Read FStartLine Write FStartLine;
    (**
      This property gets and sets the end line number for the code within the
      function.
      @precon  None.
      @postcon Gets and sets the end line number for the code within the
               function.
      @return  an Integer
    **)
    Property EndLine: Integer Read FEndLine Write FEndLine;
    (**
      A property which stores the number of statements in the method implementation.
      @precon  None.
      @postcon Returns the number of statements in the method implementation.
      @return  an Integer
    **)
    Property StmtCount : Integer Read FStmtCount Write FStmtCount;
    (**
      This property gets and sets whether the function has code profiling
      instrumentation installed.
      @precon  None.
      @postcon Gets and sets whether the function has code profiling
               instrumentation installed.
      @return  a Boolean
    **)
    Property HasProfiling: Boolean Read FHasProfiling Write FHasProfiling;
    (**
      This property gets and sets the Indent of the method.
      @precon  None.
      @postcon Gets and sets the Indent of the method.
      @return  an Integer
    **)
    Property Indent: Integer Read FIndent Write FIndent;
    (**
      A property to determine if the method is a declaration or an implementation.
      @precon  None.
      @postcon Returns true if the method is a declaration and not an implementation.
      @return  a Boolean
    **)
    Property IsDeclarationOnly : Boolean Read FIsDeclarationOnly Write FIsDeclarationOnly;
    (**
      This property gets and sets the metrics associated with the method.
      @precon  None.
      @postcon Gets and sets the metrics associated with the method.
      @param   eMetric as a TBADIModuleMetric as a constant
      @return  a Double
    **)
    Property Metric[Const eMetric : TBADIModuleMetric] : Double Read GetMetric Write SetMetric;
    (**
      This property determines which metrics are overridden with nometric or nometrics.
      @precon  None.
      @postcon Determines which metrics are overridden with nometric or nometrics.
      @return  a TBADIModuleMetrics
    **)
    Property MetricOverrides : TBADIModuleMetrics Read FMetricOverrides Write FMetricOverrides;
    (**
      This property gets the check associated with the method.
      @precon  None.
      @postcon Gets the check associated with the method.
      @param   eCheck as a TBADIModuleCheck as a constant
      @return  a Double
    **)
    Property Check[Const eCheck : TBADIModuleCheck] : Double Read GetCheck;
    (**
      This property determines which checks are overridden with nocheck or nochecks.
      @precon  None.
      @postcon Determines which metrics are overridden with nometric or nometrics.
      @return  a TBADIModuleChecks
    **)
    Property CheckOverrides : TBADIModuleChecks Read FCheckOverrides;
  End;

  (** A type to define sub classes of TGenericFunction **)
  TGenericFunctionClass = Class Of TGenericFunction;

Implementation

Uses
  {$IFDEF PROFILECODE}
  Profiler,
  {$ENDIF}
  SysUtils,
  BADI.ResourceStrings, 
  BADI.Constants,
  TypInfo,
  Math, 
  BADI.Options;

Const
  (** A unity value to increment and descending metrics. **)
  dblUnity = 1.0;
  
(**

  This method adds the given parameter to the internal list.

  @precon  AParameter must be a valid instance.
  @postcon Adds the given parameter to the internal list.

  @param   AParameter as a TGenericParameter as a constant

**)
procedure TGenericFunction.AddParameter(Const AParameter: TGenericParameter);

begin
  FParameters.Add(AParameter);
end;

(**

  This is a getter method for the Toxicity property.

  @precon  None.
  @postcon Returns a calculated toxicity for a method based on the following:
           1) Line in the Method verse Limit;
           2) Number of parameters;
           3) Number of local variables;
           4) Nested IF Depth;
           5) Cyclometric Complexity.

  @return  a Double

**)
Function TGenericFunction.CalculateToxicity: Double;

  (**

    This is a function calculate given value to a power defined in the options.

    @precon  None.
    @postcon Returns a cube of the given number.

    @param   X as a Double as a Constant
    @return  a Double

  **)
  Function F(Const X : Double) : Double;

  Begin
    Result := Math.Power(X, FToxicityPower);
  End;

  (**

    This method returns the metric divided by its limit if it is to be included in the toxicity calc.

    @precon  None
    @postcon Returns the metric divided by its limit if it is to be included in the toxicity calc.

    @param   eMetric      as a TBADIModuleMetric as a constant
    @param   eMetricSubOp as a TBADIModuleMetricSubOp as a constant
    @return  a Double

  **)
  Function G(Const eMetric : TBADIModuleMetric; Const eMetricSubOp : TBADIModuleMetricSubOp) : Double;

  Begin
    Result := 0;
    If (eMetricSubOp In FMetricSubOptions) And Not (eMetric In FMetricOverrides) Then
      Result := Metric[eMetric] / BADIOptions.ModuleMetric[eMetric].FLimit
  End;

Const
  strSender = 'Sender';

Begin
  Result := 0;
  If Not FIsDeclarationOnly Then
    Case FToxicitySummartion Of
      tsAddBeforePower:
        Begin
          Result := 0;
          Result := Result + G(mmLongMethods, mmsoToxicityIncMethodLen);
          If (ParameterCount > 0) And (CompareText(Parameters[0].Identifier, strSender) <> 0) Then
            Result := Result + G(mmLongParameterLists, mmsoToxicityIncParamLen);
          Result := Result + G(mmLongMethodVariableLists, mmsoToxicityIncVarLen);
          Result := Result + G(mmNestedIFDepth, mmsoToxicityIncIFDepth);
          Result := Result + G(mmCyclometricComplexity, mmsoToxicityIncCycloComp);
          Result := F(Result);
        End;
      tsAddAfterPower:
        Begin
          Result := 0;
          Result := Result + F(G(mmLongMethods, mmsoToxicityIncMethodLen));
          If (ParameterCount > 0) And (CompareText(Parameters[0].Identifier, strSender) <> 0) Then
            Result := Result + F(G(mmLongParameterLists, mmsoToxicityIncParamLen));
          Result := Result + F(G(mmLongMethodVariableLists, mmsoToxicityIncVarLen));
          Result := Result + F(G(mmNestedIFDepth, mmsoToxicityIncIFDepth));
          Result := Result + F(G(mmCyclometricComplexity, mmsoToxicityIncCycloComp));
        End;
    End;
End;

(**

  This method checks to see of the parameters have been referenced and if not outputs a hint.

  @precon  None.
  @postcon A hint is issued for any parameters that have not been referenced (except for event handlers
           starting with Sender).

**)
Procedure TGenericFunction.CheckReferences;

ResourceString
  strMsg = 'The parameter "%s" in method "%s" has not been referenced!';

Const
  strSender = 'Sender';

Var
  iParam: Integer;
  P: TGenericParameter;

Begin
  Inherited CheckReferences;
  If Not FIsDeclarationOnly Then
    For iParam := 0 To ParameterCount - 1 Do
      Begin
        P := Parameters[iParam];
        If (iParam = 0) And (Comparetext(Parameters[iParam].Identifier, strSender) = 0) Then
          Break;
        If Not P.Referenced Then
          AddIssue(strMsg, [P.Identifier, QualifiedName], scPublic, P.Line, P.Column, etHint, Self);
      End;
End;

(**

  This is a constructor for the TGenericFunction class.

  @precon  None.
  @postcon Creates an object list for parameters and initialises the return type to nil.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TGenericFunction.Create(Const strName: String; Const AScope: TScope; Const iLine,
  iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FParameters := TObjectList.Create(True);
  FReturnType := TGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  FStartLine := - 1;
  FEndLine := - 1;
  FIsDeclarationOnly := True;
  FIfStackDepth := 0;
  FMetricOverrides := [];
  FMetrics[mmCyclometricComplexity] := 1;
  FToxicityPower := TBADIOptions.BADIOptions.ToxicityPower;
  FToxicitySummartion := TBADIOptions.BADIOptions.ToxicitySummartion;
  FMetricSubOptions := TBADIOptions.BADIOptions.ModuleMetricSubOptions;
End;

(**

  This method decrements the IF depth stack of the method as the code is being traversed.

  @precon  None.
  @postcon The IFStackDepth is decreased.

**)
Procedure TGenericFunction.DecIFDepth;

Begin
  Dec(FIFStackDepth);
End;

(**

  This is a destructor for the TGenericFunction class.

  @precon  None.
  @postcon Frees the memory for the parameters and return type.

**)
Destructor TGenericFunction.Destroy;

Begin
  FReturnType.Free;
  FParameters.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Check property.

  @precon  None.
  @postcon Returns the given check value.

  @param   eCheck as a TBADIModuleCheck as a constant
  @return  a Double

**)
Function TGenericFunction.GetCheck(Const eCheck: TBADIModuleCheck): Double;

ResourceString
  strMsg = 'You cannot increment the check "%s"!';
  
Begin
  Case eCheck Of
    mcHardCodedIntegers, mcHardCodedNumbers, mcHardCodedStrings,
      mcUnsortedMethod..mcMissingCONSTInParemterList:
        Result := FChecks[eCheck];
  Else
    Raise Exception.CreateFmt(strMsg, [ModuleChecks[eCheck].FName]);
  End;
End;

(**

  This is a getter method for the Metric property.

  @precon  None.
  @postcon Returns the value of the given metric.

  @param   eMetric as a TBADIModuleMetric as a constant
  @return  a Double

**)
Function TGenericFunction.GetMetric(Const eMetric: TBADIModuleMetric): Double;

ResourceString
  strMsg = 'You cannot call GetMetric for %s!';

Begin
  Case eMetric Of
    mmLongMethods:
      Begin
        Result := 0;
        If FStmtCount > 0 Then
          Result := FEndLine - FStartLine + 1;
      End;
    mmLongParameterLists:        Result := ParameterCount;
    mmLongMethodVariableLists:   Result := VariableCount;
    mmNestedIFDepth:             Result := FMetrics[mmNestedIFDepth];
    mmCyclometricComplexity:     Result := FMetrics[mmCyclometricComplexity];
    mmToxicity:                  Result := CalculateToxicity;
  Else
    Raise Exception.CreateFmt(strMsg, [
      GetEnumName(TypeInfo(TBADIModuleMetric), Ord(eMetric))]);
  End;
End;

(**

  This is a getter method for the ParameterCount property.

  @precon  None.
  @postcon Returns the number of parameters associated with the method.

  @return  an Integer

**)
Function TGenericFunction.GetParameterCount: Integer;

Begin
  Result := FParameters.Count;
End;

(**

  This is a getter method for the Parameters property.

  @precon  iIndex must be a valid index.
  @postcon Returns the index instance of the paramter.

  @param   iIndex as an Integer as a constant
  @return  a TGenericParameter

**)
Function TGenericFunction.GetParameters(Const iIndex: Integer): TGenericParameter;

Begin
  Result := FParameters[iIndex] As TGenericParameter;
End;

(**

  This method increments the methods cyclometric complexity.

  @precon  None.
  @postcon The Cyclometric Complexity of the method is increased.

**)
Procedure TGenericFunction.IncCyclometricComplexity;

Begin
  FMetrics[mmCyclometricComplexity] := FMetrics[mmCyclometricComplexity] + dblUnity;
End;

(**

  This method increments the IFStackDepth as the code is being traversed and updates the IFDepth if the
  current depth is created than the IFDepth.

  @precon  None.
  @postcon Both the IFDepth and IFStackDepth are incremented.

**)
Procedure TGenericFunction.IncIFDepth;

Begin
  Inc(FIFStackDepth);
  If FIFStackDepth > FMetrics[mmNestedIFDepth]Then
    FMetrics[mmNestedIFDepth]:= FIFStackDepth;
End;

(**

  This method increments the given check and optionally marks it as overridden.

  @precon  None.
  @postcon The check is incremented if valid else an exception is raised.

  @param   eCheck         as a TBADIModuleCheck as a constant
  @param   boolOverridden as a Boolean as a constant

**)
Procedure TGenericFunction.IncrementCheck(Const eCheck: TBADIModuleCheck; Const boolOverridden: Boolean);

ResourceString
  strMsg = 'You cannot increment the check "%s"!';
  
Begin
  Case eCheck Of
    mcHardCodedIntegers, mcHardCodedNumbers, mcHardCodedStrings,
      mcUnsortedMethod..mcMissingCONSTInParemterList:
        FChecks[eCheck] := FChecks[eCheck] + dblUnity;
  Else
    Raise Exception.CreateFmt(strMsg, [ModuleChecks[eCheck].FName]);
  End;
  If boolOverridden Then
    Include(FCheckOverrides, eCheck);
End;

(**

  This method checks to see of the given token matches any parameter and if so marks the parameter as
  referenced.

  @precon  AToken must be a valid instance.
  @postcon Checks to see of the given token matches any parameter and if so marks the parameter as
           referenced.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TGenericFunction.ReferenceSymbol(Const AToken: TTokenInfo): Boolean;

Var
  iParam : Integer;
  P: TGenericParameter;

Begin
  For iParam := 0 To ParameterCount - 1 Do
    Begin
      P := Parameters[iParam];
      If CompareText(P.Identifier, AToken.Token) = 0 Then
        Begin
          P.Referenced := True;
          Result := True;
          Exit;
        End;
    End;
  Result := Inherited ReferenceSymbol(AToken);
End;

(**

  This is a setter method for the Metric property.

  @precon  None.
  @postcon Set the value of the given metric if applicable else raises an exception.

  @param   eMetric  as a TBADIModuleMetric as a constant
  @param   dblValue as a Double as a constant

**)
Procedure TGenericFunction.SetMetric(Const eMetric: TBADIModuleMetric; Const dblValue: Double);

ResourceString
  strMsg = 'You cannot set the metric type "%s"!';
  
Begin
  Case eMetric Of
    mmNestedIFDepth:         FMetrics[mmNestedIFDepth] := dblValue;
    mmCyclometricComplexity: FMetrics[mmCyclometricComplexity] := dblValue;
  Else
    Raise Exception.CreateFmt(strMsg, [ModuleMetrics[eMetric].FName]);
  End;
End;

(**

  This method returns the number of variables that are declared for the method.

  @precon  None.
  @postcon The number of method variables is returned.

  @return  a Double

**)
Function TGenericFunction.VariableCount: Double;

Var
  V : TElementContainer;
  
Begin
  Result := 0;
  V := FindElement(strVarsLabel);
  If Assigned(V) Then
    Result := V.ElementCount;
End;

End.
