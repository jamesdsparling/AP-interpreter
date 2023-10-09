using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace MathsSolverGUI
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void Form1_Load(object sender, EventArgs e)
        {

        }

        private void panel1_Paint(object sender, PaintEventArgs e)
        {

        }

        private void NumButton_Click(object sender, EventArgs e)
        {
            if (sender is Button clickedButton && clickedButton.Tag is string number)
            {
                txt_display.Text += number;
            }
        }

        private void btn_divide_Click(object sender, EventArgs e)
        {
            
        }

        private void btn_equals_Click(object sender, EventArgs e)
        {
            try
            {
                var result = MathSolverBackend.Interpreter.interpret(txt_display.Text);
                label_output.Text = "= " + result.ToString();
                label_output.ForeColor = Color.White;
            }
            catch (Exception ex)
            {
                label_output.Text = "Error: " + ex.Message;
                label_output.ForeColor = Color.Red;
            }
        }

        private void btn_backspace_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(txt_display.Text))
            {
                txt_display.Text = txt_display.Text.Substring(0, txt_display.Text.Length - 1);
            }
        }

        private void txt_display_TextChanged(object sender, EventArgs e)
        {
            label_output.ForeColor = Color.DarkGray;
        }

        private void btn_clear_Click(object sender, EventArgs e)
        {
            txt_display.Text = string.Empty;
        }
    }
}
