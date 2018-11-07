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
        procedure ChangePropertyValue(v: string);

        procedure EditOnChange(Sender: TObject);
        procedure ComboBoxOnChange(Sender: TObject);
        procedure CheckBoxOnChange(Sender: TObject);

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
    listports, vclutils, UnitServerApp, stringutils;

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
begin
    Result := true;
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

    if (length(FConfigData.prop.FList) > 0) or
      (FConfigData.prop.ValueType in [VtComportName, VtBaud]) then
    begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin

            Visible := False;
            Parent := Tree;
            Text := FConfigData.prop.FValue;
            if FConfigData.prop.ValueType = VtComportName then
                EnumComPorts(Items)
            else if FConfigData.prop.ValueType = VtBaud then
                EnumBaudRates(Items)
            else
                for i := 0 to length(FConfigData.prop.FList) - 1 do
                    Items.Add(FConfigData.prop.FList[i]);
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
            style := csDropDown;
            ItemHeight := 22;
            ItemIndex := Items.IndexOf(FConfigData.prop.FValue);
            OnChange := ComboBoxOnChange;
        end;
    end

    else if FConfigData.prop.ValueType = VtString then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin

            Visible := False;
            Parent := Tree;
            Text := FConfigData.prop.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
            OnChange := EditOnChange;

        end;
    end

    else if FConfigData.prop.ValueType = VtInt then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin

            Visible := False;
            Parent := Tree;
            Text := FConfigData.prop.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
            OnChange := EditOnChange;
        end;
    end
    else if FConfigData.prop.ValueType = VtFloat then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin

            Visible := False;
            Parent := Tree;
            Text := FConfigData.prop.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
            OnChange := EditOnChange;
        end;
    end

    else if FConfigData.prop.ValueType = VtBool then
    begin
        FEdit := TCheckBox.Create(nil);
        with FEdit as TCheckBox do
        begin

            Visible := False;
            Parent := Tree;
            Checked := not FConfigData.prop.Bool;
            CheckBoxOnChange(FEdit);
            Caption := '---';
            OnClick := CheckBoxOnChange;
        end;
    end
    else
        Result := False;
end;

procedure TPropertyEditLink.EditOnChange(Sender: TObject);
begin
    ChangePropertyValue((Sender as TEdit).Text);
end;

procedure TPropertyEditLink.ComboBoxOnChange(Sender: TObject);
begin
    ChangePropertyValue((Sender as TComboBox).Text);
end;

procedure TPropertyEditLink.CheckBoxOnChange(Sender: TObject);
begin
    FConfigData.prop.Bool := (Sender as TCheckBox).Checked;
    ChangePropertyValue(FConfigData.prop.FValue);
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

procedure TPropertyEditLink.ChangePropertyValue(v: string);
var
    pc: TChangedPropertyValue;
begin
    FConfigData.prop.SetStr(v);
    FTree.InvalidateNode(FNode);
    FTree.InvalidateNode(FNode.Parent);
    if (not FConfigData.prop.HasError) then
    begin
        pc := TChangedPropertyValue.Create(FConfigData);
        FConfigData.prop.FError := ServerApp.SetConfigProperyValue(pc);
        pc.Free;
    end;

end;

end.
