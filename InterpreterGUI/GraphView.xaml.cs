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
        private string expression;
        private PlotModel plotModel;


        public GraphView(string equation)
        {
            InitializeComponent();
            this.expression = equation;
            SetupGraph();
        }

        private List<DataPoint> ComputeGraphPoints(string expression, double minX, double maxX, double step)
        {
            var points = new List<DataPoint>();

            for (double x = minX; x <= maxX; x += step)
            {
                Number yNum = Interpreter.interpret(expression.Replace("x", x.ToString()), AngleMode.Degrees);
                double y = Interpreter.numberToFloat(yNum);
                points.Add(new DataPoint(x, y));
            }

            return points;
        }

        private void SetupGraph()
        {
            plotModel = new PlotModel { Title = "Graph of " + expression };

            var points = ComputeGraphPoints(expression, -10, 10, 1);

            var lineSeries = new LineSeries();
            foreach (var point in points)
            {
                lineSeries.Points.Add(point);
            }

            plotModel.Series.Add(lineSeries);

            plotView.Model = plotModel;
        }
    }
}
