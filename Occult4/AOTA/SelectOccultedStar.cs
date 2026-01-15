using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult;

namespace AOTA
{
	public class SelectOccultedStar : Form
	{
		private IContainer components;

		private Label label1;

		private Button cmdOK;

		internal RadioButton opt1;

		internal RadioButton opt4;

		internal RadioButton opt3;

		internal RadioButton opt2;

		private Panel panel1;

		public SelectOccultedStar(int NumStars)
		{
			InitializeComponent();
			((Control)opt4).set_Enabled(NumStars > 3);
			((Control)opt3).set_Enabled(NumStars > 2);
			((Control)opt2).set_Enabled(NumStars > 1);
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			if (opt1.get_Checked())
			{
				AOTAData.OccStar = 0;
			}
			else if (opt2.get_Checked())
			{
				AOTAData.OccStar = 1;
			}
			else if (opt3.get_Checked())
			{
				AOTAData.OccStar = 2;
			}
			else if (opt4.get_Checked())
			{
				AOTAData.OccStar = 3;
			}
			((Form)this).Close();
		}

		private void SelectOccultedStar_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		protected override void Dispose(bool disposing)
		{
			if (disposing && components != null)
			{
				components.Dispose();
			}
			((Form)this).Dispose(disposing);
		}

		private void InitializeComponent()
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			//IL_001b: Expected O, but got Unknown
			//IL_001c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0026: Expected O, but got Unknown
			//IL_0027: Unknown result type (might be due to invalid IL or missing references)
			//IL_0031: Expected O, but got Unknown
			//IL_0032: Unknown result type (might be due to invalid IL or missing references)
			//IL_003c: Expected O, but got Unknown
			//IL_003d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0047: Expected O, but got Unknown
			//IL_0048: Unknown result type (might be due to invalid IL or missing references)
			//IL_0052: Expected O, but got Unknown
			//IL_0053: Unknown result type (might be due to invalid IL or missing references)
			//IL_005d: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(SelectOccultedStar));
			opt1 = new RadioButton();
			opt4 = new RadioButton();
			opt3 = new RadioButton();
			opt2 = new RadioButton();
			label1 = new Label();
			cmdOK = new Button();
			panel1 = new Panel();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)opt1).set_AutoSize(true);
			opt1.set_Checked(true);
			((Control)opt1).set_Font(new Font("Consolas", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)opt1).set_Location(new Point(16, 85));
			((Control)opt1).set_Name("opt1");
			((Control)opt1).set_Size(new Size(67, 18));
			((Control)opt1).set_TabIndex(0);
			opt1.set_TabStop(true);
			((Control)opt1).set_Text("Star 1");
			((ButtonBase)opt1).set_UseVisualStyleBackColor(true);
			((Control)opt4).set_AutoSize(true);
			((Control)opt4).set_Enabled(false);
			((Control)opt4).set_Font(new Font("Consolas", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)opt4).set_Location(new Point(16, 178));
			((Control)opt4).set_Name("opt4");
			((Control)opt4).set_Size(new Size(67, 18));
			((Control)opt4).set_TabIndex(1);
			opt4.set_TabStop(true);
			((Control)opt4).set_Text("Star 4");
			((ButtonBase)opt4).set_UseVisualStyleBackColor(true);
			((Control)opt3).set_AutoSize(true);
			((Control)opt3).set_Enabled(false);
			((Control)opt3).set_Font(new Font("Consolas", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)opt3).set_Location(new Point(16, 147));
			((Control)opt3).set_Name("opt3");
			((Control)opt3).set_Size(new Size(67, 18));
			((Control)opt3).set_TabIndex(2);
			opt3.set_TabStop(true);
			((Control)opt3).set_Text("Star 3");
			((ButtonBase)opt3).set_UseVisualStyleBackColor(true);
			((Control)opt2).set_AutoSize(true);
			((Control)opt2).set_Enabled(false);
			((Control)opt2).set_Font(new Font("Consolas", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)opt2).set_Location(new Point(16, 116));
			((Control)opt2).set_Name("opt2");
			((Control)opt2).set_Size(new Size(67, 18));
			((Control)opt2).set_TabIndex(3);
			opt2.set_TabStop(true);
			((Control)opt2).set_Text("Star 2");
			((ButtonBase)opt2).set_UseVisualStyleBackColor(true);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(22, 5));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(302, 60));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("Specify the star that is occulted from the list below. The\r\nlabels will correspond to the labels in the measuring\r\ntool. Typically the Occulted star will be labelled\r\n      signal - target");
			((Control)cmdOK).set_Location(new Point(136, 205));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(76, 36));
			((Control)cmdOK).set_TabIndex(5);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)label1);
			((Control)panel1).set_Location(new Point(-1, -1));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(351, 73));
			((Control)panel1).set_TabIndex(6);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(7f, 15f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(349, 246));
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)opt2);
			((Control)this).get_Controls().Add((Control)(object)opt3);
			((Control)this).get_Controls().Add((Control)(object)opt4);
			((Control)this).get_Controls().Add((Control)(object)opt1);
			((Control)this).set_Font(new Font("Consolas", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("SelectOccultedStar");
			((Control)this).set_Text("Select the star that was occulted");
			((Form)this).add_Load((EventHandler)SelectOccultedStar_Load);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
