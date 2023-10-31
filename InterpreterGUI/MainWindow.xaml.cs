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
        }

        private void RunInterpreter()
        {
                if (!(string.IsNullOrWhiteSpace(txtInput.Text)))
                {
                    try
                    {
                        var result = Interpreter.interpret(txtInput.Text, mode: Interpreter.AngleMode.Radians);
                        labOutput.Content = result;
                        labOutput.Foreground = new SolidColorBrush(Colors.White);
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
            } else
            {
                labOutput.Foreground = new SolidColorBrush(Colors.Gray);
            }
        }
    }
}
