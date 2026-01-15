using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class MonitorScale : Form
	{
		private int[] Values = new int[9] { 100, 125, 150, 175, 200, 225, 250, 300, 350 };

		private IContainer components;

		private RadioButton opt100;

		private RadioButton opt125;

		private RadioButton opt200;

		private RadioButton opt225;

		private RadioButton opt300;

		private RadioButton opt350;

		private RadioButton opt250;

		private RadioButton opt175;

		private RadioButton opt150;

		private Panel panel1;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		public MonitorScale()
		{
			InitializeComponent();
		}

		private void MonitorScale_FormClosing(object sender, FormClosingEventArgs e)
		{
			if (opt100.get_Checked())
			{
				Settings.Default.MonitorScale = 100;
			}
			else if (opt125.get_Checked())
			{
				Settings.Default.MonitorScale = 125;
			}
			else if (opt150.get_Checked())
			{
				Settings.Default.MonitorScale = 150;
			}
			else if (opt175.get_Checked())
			{
				Settings.Default.MonitorScale = 175;
			}
			else if (opt200.get_Checked())
			{
				Settings.Default.MonitorScale = 200;
			}
			else if (opt225.get_Checked())
			{
				Settings.Default.MonitorScale = 225;
			}
			else if (opt250.get_Checked())
			{
				Settings.Default.MonitorScale = 250;
			}
			else if (opt300.get_Checked())
			{
				Settings.Default.MonitorScale = 300;
			}
			else if (opt350.get_Checked())
			{
				Settings.Default.MonitorScale = 350;
			}
		}

		private void MonitorScale_Load(object sender, EventArgs e)
		{
			int monitorScale = Settings.Default.MonitorScale;
			opt100.set_Checked(monitorScale == 100);
			opt125.set_Checked(monitorScale == 125);
			opt150.set_Checked(monitorScale == 150);
			opt175.set_Checked(monitorScale == 175);
			opt200.set_Checked(monitorScale == 200);
			opt225.set_Checked(monitorScale == 225);
			opt250.set_Checked(monitorScale == 250);
			opt300.set_Checked(monitorScale == 300);
			opt350.set_Checked(monitorScale == 350);
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
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Expected O, but got Unknown
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_0021: Expected O, but got Unknown
			//IL_0022: Unknown result type (might be due to invalid IL or missing references)
			//IL_002c: Expected O, but got Unknown
			//IL_002d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0037: Expected O, but got Unknown
			//IL_0038: Unknown result type (might be due to invalid IL or missing references)
			//IL_0042: Expected O, but got Unknown
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_004d: Expected O, but got Unknown
			//IL_004e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0058: Expected O, but got Unknown
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			//IL_0063: Expected O, but got Unknown
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006e: Expected O, but got Unknown
			//IL_006f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0079: Expected O, but got Unknown
			//IL_007a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0084: Expected O, but got Unknown
			//IL_0085: Unknown result type (might be due to invalid IL or missing references)
			//IL_008f: Expected O, but got Unknown
			//IL_0090: Unknown result type (might be due to invalid IL or missing references)
			//IL_009a: Expected O, but got Unknown
			//IL_0856: Unknown result type (might be due to invalid IL or missing references)
			//IL_0860: Expected O, but got Unknown
			opt100 = new RadioButton();
			opt125 = new RadioButton();
			opt200 = new RadioButton();
			opt225 = new RadioButton();
			opt300 = new RadioButton();
			opt350 = new RadioButton();
			opt250 = new RadioButton();
			opt175 = new RadioButton();
			opt150 = new RadioButton();
			panel1 = new Panel();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			((Control)panel1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)opt100).set_AutoSize(true);
			((Control)opt100).set_Location(new Point(10, 7));
			((Control)opt100).set_Name("opt100");
			((Control)opt100).set_Size(new Size(51, 17));
			((Control)opt100).set_TabIndex(0);
			opt100.set_TabStop(true);
			((Control)opt100).set_Text("100%");
			((ButtonBase)opt100).set_UseVisualStyleBackColor(true);
			((Control)opt125).set_AutoSize(true);
			((Control)opt125).set_Location(new Point(10, 30));
			((Control)opt125).set_Name("opt125");
			((Control)opt125).set_Size(new Size(51, 17));
			((Control)opt125).set_TabIndex(1);
			opt125.set_TabStop(true);
			((Control)opt125).set_Text("125%");
			((ButtonBase)opt125).set_UseVisualStyleBackColor(true);
			((Control)opt200).set_AutoSize(true);
			((Control)opt200).set_Location(new Point(10, 99));
			((Control)opt200).set_Name("opt200");
			((Control)opt200).set_Size(new Size(51, 17));
			((Control)opt200).set_TabIndex(2);
			opt200.set_TabStop(true);
			((Control)opt200).set_Text("200%");
			((ButtonBase)opt200).set_UseVisualStyleBackColor(true);
			((Control)opt225).set_AutoSize(true);
			((Control)opt225).set_Location(new Point(10, 122));
			((Control)opt225).set_Name("opt225");
			((Control)opt225).set_Size(new Size(51, 17));
			((Control)opt225).set_TabIndex(3);
			opt225.set_TabStop(true);
			((Control)opt225).set_Text("225%");
			((ButtonBase)opt225).set_UseVisualStyleBackColor(true);
			((Control)opt300).set_AutoSize(true);
			((Control)opt300).set_Location(new Point(10, 168));
			((Control)opt300).set_Name("opt300");
			((Control)opt300).set_Size(new Size(51, 17));
			((Control)opt300).set_TabIndex(4);
			opt300.set_TabStop(true);
			((Control)opt300).set_Text("300%");
			((ButtonBase)opt300).set_UseVisualStyleBackColor(true);
			((Control)opt350).set_AutoSize(true);
			((Control)opt350).set_Location(new Point(10, 191));
			((Control)opt350).set_Name("opt350");
			((Control)opt350).set_Size(new Size(51, 17));
			((Control)opt350).set_TabIndex(8);
			opt350.set_TabStop(true);
			((Control)opt350).set_Text("350%");
			((ButtonBase)opt350).set_UseVisualStyleBackColor(true);
			((Control)opt250).set_AutoSize(true);
			((Control)opt250).set_Location(new Point(10, 145));
			((Control)opt250).set_Name("opt250");
			((Control)opt250).set_Size(new Size(51, 17));
			((Control)opt250).set_TabIndex(7);
			opt250.set_TabStop(true);
			((Control)opt250).set_Text("250%");
			((ButtonBase)opt250).set_UseVisualStyleBackColor(true);
			((Control)opt175).set_AutoSize(true);
			((Control)opt175).set_Location(new Point(10, 76));
			((Control)opt175).set_Name("opt175");
			((Control)opt175).set_Size(new Size(51, 17));
			((Control)opt175).set_TabIndex(6);
			opt175.set_TabStop(true);
			((Control)opt175).set_Text("175%");
			((ButtonBase)opt175).set_UseVisualStyleBackColor(true);
			((Control)opt150).set_AutoSize(true);
			((Control)opt150).set_Location(new Point(10, 53));
			((Control)opt150).set_Name("opt150");
			((Control)opt150).set_Size(new Size(51, 17));
			((Control)opt150).set_TabIndex(5);
			opt150.set_TabStop(true);
			((Control)opt150).set_Text("150%");
			((ButtonBase)opt150).set_UseVisualStyleBackColor(true);
			((Control)panel1).set_BackColor(Color.OldLace);
			panel1.set_BorderStyle((BorderStyle)2);
			((Control)panel1).get_Controls().Add((Control)(object)opt350);
			((Control)panel1).get_Controls().Add((Control)(object)opt250);
			((Control)panel1).get_Controls().Add((Control)(object)opt175);
			((Control)panel1).get_Controls().Add((Control)(object)opt150);
			((Control)panel1).get_Controls().Add((Control)(object)opt300);
			((Control)panel1).get_Controls().Add((Control)(object)opt225);
			((Control)panel1).get_Controls().Add((Control)(object)opt200);
			((Control)panel1).get_Controls().Add((Control)(object)opt125);
			((Control)panel1).get_Controls().Add((Control)(object)opt100);
			((Control)panel1).set_Location(new Point(14, 15));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(74, 221));
			((Control)panel1).set_TabIndex(9);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(111, 29));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(250, 39));
			((Control)label1).set_TabIndex(10);
			((Control)label1).set_Text("When Occult needs to save  FORM  (not an image)\r\nthe scale factor used by the monitor(s) must be\r\ntaken into account");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(111, 89));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(198, 39));
			((Control)label2).set_TabIndex(11);
			((Control)label2).set_Text("The Monitor scale is usually set at 100%.\r\nHowever a larger scale may be set if you\r\nare using high-resolution monitors");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(111, 149));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(249, 39));
			((Control)label3).set_TabIndex(12);
			((Control)label3).set_Text("The current Monitor scale can be found on\r\nyour computer under Settings, System, Display,\r\nScale and layout - size of text, apps and other items");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(111, 209));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(226, 13));
			((Control)label4).set_TabIndex(13);
			((Control)label4).set_Text("Select the value at left that matches that scale");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(369, 249));
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)5);
			((Control)this).set_Name("MonitorScale");
			((Control)this).set_Text("Set scale for Form saves");
			((Form)this).add_FormClosing(new FormClosingEventHandler(MonitorScale_FormClosing));
			((Form)this).add_Load((EventHandler)MonitorScale_Load);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
