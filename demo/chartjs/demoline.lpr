program demoline;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset, d2: TChartLineDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'line';
  config.data := TChartData.new;
  config.data.labels := ['January', 'February', 'March', 'April', 'May',
    'June', 'July'];
  dataset := TChartLineDataset.new;
  dataset.label_ := 'My First dataset';
  dataset.data := [65, 59, 80, 81, 56, 55, 40];
  dataset.fill := False;
  dataset.borderColor := 'rgb(75, 192, 192)';
  dataset.lineTension := 0.1;
  d2 := TChartLineDataset.new;
  d2.label_ := 'My other dataset';
  d2.data := [35, 49, 90, 70, 55, 45, 20];
  d2.fill := False;
  d2.borderColor := 'rgb(255, 192, 192)';
  d2.lineTension := 0.1;
  config.data.datasets := [dataset,d2];
  TChart.new('myChart', config);
end.
