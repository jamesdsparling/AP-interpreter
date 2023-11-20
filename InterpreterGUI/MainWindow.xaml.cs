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
        public MainWindow()
        {
            InitializeComponent();
            this.DataContext = new SymbolViewModel();
        }

        private void RunInterpreter()
        {
            if (txtInput == null || radioDegrees == null || labOutput == null) { return; }
            if (string.IsNullOrWhiteSpace(txtInput.Text)) { return; }

            try
            {
                var mode = radioDegrees.IsChecked ?? true ?
                    Interpreter.AngleMode.Degrees :
                    Interpreter.AngleMode.Radians;
                var result = Interpreter.interpret(txtInput.Text, mode: mode);
                labOutput.Content = "= " + result.ToString();
                labOutput.Foreground = new SolidColorBrush(Colors.White);
                var viewModel = this.DataContext as SymbolViewModel;
                viewModel?.UpdateSymbols();
            }
            catch (Exception ex)
            {
                labOutput.Content = "Error: " + ex.Message;
                labOutput.Foreground = new SolidColorBrush(Colors.Red);
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
    }
}