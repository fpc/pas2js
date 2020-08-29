unit demohtmlwidgets;

{$mode objfpc}

interface

uses
  sysutils, web, js, webwidget, htmlwidgets,  widgetdemo;

Type
  
  { TButtonDemo }

  TButtonDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TRadioDemo }

  TRadioDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TCheckboxDemo }

  TCheckboxDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextInputDemo }

  TTextInputDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;
  
  { TFileInputDemo }

  TFileInputDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TDateInputDemo }

  TDateInputDemo = class(TDemoContainer)
  protected
    procedure DoChange(Sender: TObject; Event: TJSEvent); override;
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TButtonInputDemo }

  TButtonInputDemo = class(TDemoContainer)
  private
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { THiddenInputDemo }

  THiddenInputDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextAreaDemo }

  TTextAreaDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TLabelWidgetDemo }

  TLabelWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextWidgetDemo }

  TTextWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextLinesWidgetDemo }

  TTextLinesWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;


implementation

uses democonsts;

{ TTextLinesWidgetDemo }

class function TTextLinesWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextLinesWidget;
end;

procedure TTextLinesWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
  With TTextLinesWidget(WidgetInstance).Lines do
    begin
    beginUpdate;
    try
      Add(Lorem1);
      Add('');
      Add(Lorem2);
      Add('');
      Add(Lorem3);
    finally
      EndUpdate;
    end;
    end;
end;

{ TTextWidgetDemo }

class function TTextWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextWidget
end;

procedure TTextWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
  TTextWidget(WidgetInstance).Text:=Lorem1;
end;

{ TLabelWidgetDemo }

class function TLabelWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TLabelwidget;
end;

procedure TLabelWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
  TLabelwidget(WidgetInstance).Text:=Lorem1;
end;

{ TTextAreaDemo }

class function TTextAreaDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextAreaWidget;
end;

procedure TTextAreaDemo.ShowDemo;
begin
  inherited ShowDemo;
  TTextAreaWidget(WidgetInstance).Rows:=20;
  TTextAreaWidget(WidgetInstance).Columns:=80;
  TTextAreaWidget(WidgetInstance).Lines.Add(Lorem1);
  TTextAreaWidget(WidgetInstance).Lines.Add('');
  TTextAreaWidget(WidgetInstance).Lines.Add(Lorem2);
  TTextAreaWidget(WidgetInstance).Lines.Add('');
  TTextAreaWidget(WidgetInstance).Lines.Add(Lorem3);
end;

{ THiddenInputDemo }

class function THiddenInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=THiddenInputWidget;
end;

procedure THiddenInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  THiddenInputWidget(WidgetInstance).Value:='This value is hidden';
  THiddenInputWidget(WidgetInstance).ValueName:='MyHidden';
end;

{ TButtonDemo }

class function TButtonDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TButtonWidget;
end;

procedure TButtonDemo.ShowDemo;
begin
  inherited ShowDemo;
  WidgetInstance.OnClick:=@DoClick;
end;

{ TRadioDemo }


class function TRadioDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TRadioInputWidget;
end;

procedure TRadioDemo.ShowDemo;
begin
  inherited ShowDemo;
  TRadioInputWidget(WidgetInstance).Text:='A Radio';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TCheckboxDemo }

class function TCheckboxDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TCheckboxInputWidget;
end;

procedure TCheckboxDemo.ShowDemo;
begin
  inherited ShowDemo;
  TCheckboxInputWidget(WidgetInstance).Text:='A checkbox';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TTextInputDemo }

class function TTextInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextInputWidget;
end;

procedure TTextInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  TTextInputWidget(WidgetInstance).Value:='A Text Value';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TDateInputDemo }

procedure TDateInputDemo.DoChange(Sender: TObject; Event: TJSEvent);
begin
  Inherited;
  Writeln(Sender.ClassName,' date value: ', DateToStr(TDateInputWidget(WidgetInstance).Date));
end;

class function TDateInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TDateInputWidget;
end;

procedure TDateInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  TDateInputWidget(WidgetInstance).Date:=Date+1;
  WidgetInstance.OnChange:=@DoChange;
end;

{ TFileInputDemo }


class function TFileInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TFileInputWidget;
end;

procedure TFileInputDemo.ShowDemo;
begin
  inherited ShowDemo;
//  TFileInputWidget(WidgetInstance).FileName:='my.txt';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TButtonInputDemo }


class function TButtonInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TButtonInputWidget;
end;

procedure TButtonInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  WidgetInstance.OnClick:=@DoClick;
  TButtonInputWidget(WidgetInstance).Value:='Press me';
end;


initialization
  TCheckboxDemo.RegisterDemo;
  TRadioDemo.RegisterDemo;
  TButtonDemo.RegisterDemo;
  TTextInputDemo.RegisterDemo;
  TDateInputDemo.RegisterDemo;
  TFileInputDemo.RegisterDemo;
  TButtonInputDemo.RegisterDemo;
  THiddenInputDemo.RegisterDemo;
  TTextAreaDemo.RegisterDemo;
  TLabelWidgetDemo.RegisterDemo;
  TTextWidgetDemo.RegisterDemo;
  TTextLinesWidgetDemo.RegisterDemo;
end.

