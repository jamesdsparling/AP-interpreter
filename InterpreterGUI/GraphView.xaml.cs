using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;
using InterpreterBackend;
using OxyPlot;
using OxyPlot.Series;
using static InterpreterBackend.Interpreter;

namespace InterpreterGUI
{
    /// <summary>
    /// Interaction logic for GraphView.xaml
    /// </summary>
    public partial class GraphView : Window
    {
        private PlotModel plotModel;


        public GraphView()
        {
            InitializeComponent();
            plotModel = new PlotModel { Title = "GraphView" };
        }

        private List<DataPoint> ComputeGraphPoints(string expression, double minX, double maxX, double step)
        {
            var points = new List<DataPoint>();

            for (double x = minX; x <= maxX; x += step)
            {
                string xAsString = x.ToString("0.#############################");
                try
                {
                    Number yNum = Interpreter.interpret(expression.Replace("x", "(" + xAsString + ")"), AngleMode.Degrees);
                    double y = Interpreter.numberToFloat(yNum);
                    points.Add(new DataPoint(x, y));
                } catch (Exception ex)
                {
                    // TODO: handle error
                }
            }

            return points;
        }

        public void SetupGraphForRange(string expression, int range1, int range2, double step)
        {
            var points = ComputeGraphPoints(expression, range1, range2, step);

            var lineSeries = new LineSeries();
            foreach (var point in points)
            {
                lineSeries.Points.Add(point);
            }

            plotModel.Series.Add(lineSeries);

            plotView.InvalidatePlot(true);
            plotView.Model = plotModel;
        }
        public void SetupGraph(string expression)
        {
            var points = ComputeGraphPoints(expression, -10, 10, 0.01);

            var lineSeries = new LineSeries();
            foreach (var point in points)
            {
                lineSeries.Points.Add(point);
            }

            plotModel.Series.Add(lineSeries);

            plotView.InvalidatePlot(true);
            plotView.Model = plotModel;
        }

    }
}
