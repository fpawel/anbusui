unit PropertyValueEditors;

// Utility unit for the advanced Virtual Treeview demo application which contains the implementation of edit link
// interfaces used in other samples of the demo.

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, VirtualTrees, ExtDlgs, ImgList, Buttons, ExtCtrls, ComCtrls,
    Mask, model_config;

type


    // ----------------------------------------------------------------------------------------------------------------------
    // Our own edit link to implement several different node editors.
    TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
    private
        FEdit: TWinControl; // One of the property editor classes.
        FTree: TVirtualStringTree; // A back reference to the tree calling.
        FNode: PVirtualNode; // The node being edited.
        FColumn: integer; // The column of the node being edited.
        FConfigData: PConfigData;
        procedure DoPropertyValueChanged;
    protected
        procedure EditKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
        procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    public
        destructor Destroy; override;

        function BeginEdit: boolean; stdcall;
        function CancelEdit: boolean; stdcall;
        function EndEdit: boolean; stdcall;
        function GetBounds: TRect; stdcall;
        function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex): boolean; stdcall;
        procedure ProcessMessage(var Message: TMessage); stdcall;
        procedure SetBounds(R: TRect); stdcall;
        
    end;

    // ----------------------------------------------------------------------------------------------------------------------

type

    TPropertyTextKind = (ptkText, ptkHint);

    // The following constants provide the property tree with default data.



    // ----------------------------------------------------------------------------------------------------------------------

implementation

uses
    listports, vclutils, UnitServerApp;

// ----------------- TPropertyEditLink ----------------------------------------------------------------------------------

// This implementation is used in VST3 to make a connection beween the tree
// and the actual edit window which might be a simple edit, a combobox
// or a memo etc.

destructor TPropertyEditLink.Destroy;

begin
    // FEdit.Free; casues issue #357. Fix:
    if FEdit.HandleAllocated then
        PostMessage(FEdit.Handle, CM_RELEASE, 0, 0);
    inherited;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var
    CanAdvance: boolean;

begin
    CanAdvance := true;

    case Key of
        VK_ESCAPE:
            begin
                Key := 0; // ESC will be handled in EditKeyUp()
            end;
        VK_RETURN:
            if CanAdvance then
            begin
                FTree.EndEditNode;
                Key := 0;
            end;

        VK_UP, VK_DOWN:
            begin
                // Consider special cases before finishing edit mode.
                CanAdvance := Shift = [];
                if FEdit is TComboBox then
                    CanAdvance := CanAdvance and not TComboBox(FEdit)
                      .DroppedDown;
                if FEdit is TDateTimePicker then
                    CanAdvance := CanAdvance and not TDateTimePicker(FEdit)
                      .DroppedDown;

                if CanAdvance then
                begin
                    // Forward the keypress to the tree. It will asynchronously change the focused node.
                    PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
                    Key := 0;
                end;
            end;
    end;
end;

procedure TPropertyEditLink.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    case Key of
        VK_ESCAPE:
            begin
                FTree.CancelEditNode;
                Key := 0;
            end; // VK_ESCAPE
    end; // case
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.BeginEdit: boolean;

begin
    Result := true;
    FEdit.Show;
    FEdit.SetFocus;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.CancelEdit: boolean;

begin
    Result := true;
    FEdit.Hide;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.EndEdit: boolean;

var
    prop: TConfigProperty;
    Buffer: array [0 .. 1024] of Char;
    S: UnicodeString;
begin
    Result := true;

    prop := PConfigData(FTree.GetNodeData(FNode)).prop;
    if FEdit is TComboBox then
        S := TComboBox(FEdit).Text
    else if FEdit is TCheckBox then
    begin
        if (FEdit as TCheckBox).Checked then
            S := '1'
        else
            S := '0';
    end
    else
    begin
        GetWindowText(FEdit.Handle, Buffer, 1024);
        S := Buffer;
    end;

    if S <> prop.FValue then
    begin
        prop.SetStr(S);
        DoPropertyValueChanged;

        // DataModule1.UpdateConfigPropertyValue(Data.FSectionName, Data.FPropertyName, Data.FValue);
        FTree.InvalidateNode(FNode);
        FTree.InvalidateNode(FNode.Parent);
    end;
    FEdit.Hide;
    try
        FTree.SetFocus;
    except

    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.GetBounds: TRect;

begin
    Result := FEdit.BoundsRect;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean;

var
    i: integer;

begin
    Result := true;
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;
    FConfigData := FTree.GetNodeData(FNode);

    // determine what edit type actually is needed
    FEdit.Free;
    FEdit := nil;

    if (length(FConfigData.Prop.FList) > 0) or (FConfigData.Prop.ValueType in
        [ VtComportName, VtBaud ] ) then
    begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
            Visible := False;
            Parent := Tree;
            Text := FConfigData.Prop.FValue;
            if FConfigData.Prop.ValueType = VtComportName then
                EnumComPorts(Items)
            else if FConfigData.Prop.ValueType = VtBaud then
                EnumBaudRates(Items)
            else
                for i := 0 to length(FConfigData.Prop.FList) - 1 do
                    Items.Add(FConfigData.Prop.FList[i]);
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
            style := csDropDown;


            ItemHeight := 22;
            ItemIndex := Items.IndexOf(FConfigData.Prop.FValue);
        end;
    end

    else if FConfigData.Prop.ValueType = VtString then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
            Visible := False;
            Parent := Tree;
            Text := FConfigData.Prop.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
        end;
    end

    else if FConfigData.Prop.ValueType = VtInt then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
            Visible := False;
            Parent := Tree;
            Text := FConfigData.Prop.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
        end;
    end
    else if FConfigData.Prop.ValueType = VtFloat then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
            Visible := False;
            Parent := Tree;
            Text := FConfigData.Prop.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
        end;
    end

    else if FConfigData.Prop.ValueType = VtBool then
    begin
        FEdit := TCheckBox.Create(nil);
        with FEdit as TCheckBox do
        begin
            Visible := False;
            Parent := Tree;
            if FConfigData.Prop.FValue = '0' then
                FConfigData.Prop.FValue := '1'
            else
                FConfigData.Prop.FValue := '0';

            Checked := FConfigData.Prop.FValue <> '0';
            Caption := '---';

            FConfigData.Prop.SetStr(FConfigData.Prop.FValue);
            DoPropertyValueChanged;

        end;
    end
    else
        Result := False;
end;


// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);

begin
    FEdit.WindowProc(Message);
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.SetBounds(R: TRect);

var
    Dummy: integer;

begin
    // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
    // we have to set the edit's width explicitly to the width of the column.
    FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
    FEdit.BoundsRect := R;
end;
// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.DoPropertyValueChanged;
var
    pc: TChangedPropertyValue;
begin
    if (not FConfigData.Prop.HasError) then
    begin
        pc := TChangedPropertyValue.Create(FConfigData);
        FConfigData.Prop.FError := ServerApp.SetConfigProperyValue(pc);
        pc.Free;
    end;

end;

end.
