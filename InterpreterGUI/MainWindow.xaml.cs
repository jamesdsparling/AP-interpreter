using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using InterpreterBackend;
using static InterpreterBackend.Interpreter;

namespace InterpreterGUI
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private GraphView graphView;

        public MainWindow()
        {
            InitializeComponent();
            this.graphView = new GraphView();
            this.DataContext = new SymbolViewModel();
        }

        private void RunInterpreter()
        {
            if (txtInput == null || radioDegrees == null || labOutput == null) { return; }
            if (string.IsNullOrWhiteSpace(txtInput.Text)) { return; }

            var matchExpForRange    = Regex.Match(txtInput.Text, @"^y\s*=\s*(.*)\s*for\s(-?\d+)\s*(<|<=)\s*x\s*(<|<=)\s*(-?\d+);\s*(\d+[.]\d+|\d+$)", RegexOptions.IgnoreCase);
            var matchExpFor         = Regex.Match(txtInput.Text, @"^y\s*=\s*(.*)\s*for\s(-?\d+)\s*(<|<=)\s*x\s*(<|<=)\s*(-?\d+)", RegexOptions.IgnoreCase);
            var matchExp            = Regex.Match(txtInput.Text, @"^y\s*=\s*(.*)", RegexOptions.IgnoreCase);
            if (matchExpForRange.Success)
            {
                // Extracting everything after 'y=', 'for' and increment
                string equation = matchExpForRange.Groups[1].Value;
                int range1 = Convert.ToInt32(matchExpForRange.Groups[2].Value);
                string rangeSymbol1 = matchExpForRange.Groups[3].Value;
                string rangeSymbol2 = matchExpForRange.Groups[4].Value;
                int range2 = Convert.ToInt32(matchExpForRange.Groups[5].Value);
                double step = double.Parse(matchExpForRange.Groups[6].Value);
                if (rangeSymbol1 == "<"){range1++;}
                if (rangeSymbol2 == "<"){range2--;}
                if (!this.graphView.IsLoaded)
                {
                    this.graphView = new GraphView();
                }
                this.graphView.SetupGraphForRange(equation, range1, range2, step);
                graphView.Show();
                labOutput.Content = "Plotting expression, please wait...";
                labOutput.Foreground = new SolidColorBrush(Colors.Orange);
            }
            else if (matchExpFor.Success)
            {
                // Extracting everything after 'y=' and 'for'
                string equation = matchExpFor.Groups[1].Value;
                int range1 = Convert.ToInt32(matchExpFor.Groups[2].Value);
                string rangeSymbol1 = matchExpFor.Groups[3].Value;
                string rangeSymbol2 = matchExpFor.Groups[4].Value;
                int range2 = Convert.ToInt32(matchExpFor.Groups[5].Value);
                if (!this.graphView.IsLoaded)
                {
                    this.graphView = new GraphView();
                }
                double step = 0.01;
                this.graphView.SetupGraphForRange(equation, range1, range2, step);
                graphView.Show();
                labOutput.Content = "Plotting expression, please wait...";
                labOutput.Foreground = new SolidColorBrush(Colors.Orange);
            }
            else if (matchExp.Success)
            {
                // Extracting everything after 'y='
                string equation = matchExp.Groups[1].Value;
                if (!this.graphView.IsLoaded)
                {
                    this.graphView = new GraphView();
                }
                this.graphView.SetupGraph(equation);
                graphView.Show();
                labOutput.Content = "Plotting expression, please wait...";
                labOutput.Foreground = new SolidColorBrush(Colors.Orange);
            }
            else
            {

                try
                {
                    if (txtInput.Text.Contains("for"))
                    {
                        var result = Interpreter.interpretControlFlow(txtInput.Text);
                        labOutput.Content = "= " + result.ToString();
                        labOutput.Foreground = new SolidColorBrush(Colors.White);
                    }
                    else
                    {
                        var mode = radioDegrees.IsChecked ?? true ?
                            Interpreter.AngleMode.Degrees :
                            Interpreter.AngleMode.Radians;
                        var result = Interpreter.interpret(txtInput.Text, mode: mode);
                        labOutput.Content = "= " + result.ToString();
                        labOutput.Foreground = new SolidColorBrush(Colors.White);
                    }

                    var viewModel = this.DataContext as SymbolViewModel;
                    viewModel?.UpdateSymbols();
                }
                catch (Exception ex)
                {
                    labOutput.Content = "Error: " + ex.Message;
                    labOutput.Foreground = new SolidColorBrush(Colors.Red);
                }
            }

        }
        private void TextBox_KeyDown(object sender, KeyEventArgs e)
        {
            if (e.Key == Key.Enter)
            {
                RunInterpreter();
            }
        }

        private void TextBox_TextChanged(object sender, TextChangedEventArgs e)
        {
            if (labOutput == null) { return; }
            labOutput.Foreground = new SolidColorBrush(Colors.Gray);
        }

        private void AngleChanged(object sender, RoutedEventArgs e)
        {
            RunInterpreter();
        }

        private void Equ_Click(object sender, RoutedEventArgs e)
        {
            RunInterpreter();
        }
        private void Button_Click(object sender, RoutedEventArgs e)
        {
            if (sender is Button button)
            {
                string buttonText = button.Content.ToString();
                txtInput.Text += buttonText;
            }
        }
        private void Clear_Click(object sender, RoutedEventArgs e)
        {
            txtInput.Text = "";
        }

        private void Backspace_Click(object sender, RoutedEventArgs e)
        {
            txtInput.Text = txtInput.Text[..^1];
        }

        private void BtnGotFocus(object sender, RoutedEventArgs e)
        {
            txtInput.Focus();
        }
    }
}
