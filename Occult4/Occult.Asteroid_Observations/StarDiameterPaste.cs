using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class StarDiameterPaste : Form
	{
		private IContainer components;

		private GroupBox groupBox1;

		private RadioButton optTangra;

		private RadioButton optLiMovie;

		private GroupBox groupBox2;

		private RadioButton opt3;

		private RadioButton opt4;

		private RadioButton opt2;

		private RadioButton opt1;

		private Button cmdOK;

		private Button cndCancel;

		public StarDiameterPaste()
		{
			InitializeComponent();
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)1);
			((Form)this).Close();
		}

		private void cndCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
			((Form)this).Close();
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
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_03af: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b9: Expected O, but got Unknown
			//IL_0456: Unknown result type (might be due to invalid IL or missing references)
			//IL_0460: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_05b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ba: Expected O, but got Unknown
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			groupBox1 = new GroupBox();
			groupBox2 = new GroupBox();
			cmdOK = new Button();
			cndCancel = new Button();
			opt3 = new RadioButton();
			opt4 = new RadioButton();
			opt2 = new RadioButton();
			opt1 = new RadioButton();
			optTangra = new RadioButton();
			optLiMovie = new RadioButton();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox1).get_Controls().Add((Control)(object)optTangra);
			((Control)groupBox1).get_Controls().Add((Control)(object)optLiMovie);
			((Control)groupBox1).set_Location(new Point(9, 13));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(200, 42));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Program used to create CSV file");
			((Control)groupBox2).get_Controls().Add((Control)(object)opt3);
			((Control)groupBox2).get_Controls().Add((Control)(object)opt4);
			((Control)groupBox2).get_Controls().Add((Control)(object)opt2);
			((Control)groupBox2).get_Controls().Add((Control)(object)opt1);
			((Control)groupBox2).set_Location(new Point(9, 77));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(200, 69));
			((Control)groupBox2).set_TabIndex(1);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("Occulted start in CSV file is:");
			((Control)cmdOK).set_Location(new Point(36, 163));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(57, 23));
			((Control)cmdOK).set_TabIndex(2);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cndCancel).set_Location(new Point(130, 163));
			((Control)cndCancel).set_Name("cndCancel");
			((Control)cndCancel).set_Size(new Size(57, 23));
			((Control)cndCancel).set_TabIndex(3);
			((Control)cndCancel).set_Text("Cancel");
			((ButtonBase)cndCancel).set_UseVisualStyleBackColor(true);
			((Control)cndCancel).add_Click((EventHandler)cndCancel_Click);
			((Control)opt3).set_AutoSize(true);
			opt3.set_Checked(Settings.Default.CSVPaste3);
			((Control)opt3).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CSVPaste3", true, (DataSourceUpdateMode)1));
			((Control)opt3).set_Location(new Point(20, 46));
			((Control)opt3).set_Name("opt3");
			((Control)opt3).set_Size(new Size(65, 17));
			((Control)opt3).set_TabIndex(3);
			((Control)opt3).set_Text("Object 3");
			((ButtonBase)opt3).set_UseVisualStyleBackColor(true);
			((Control)opt4).set_AutoSize(true);
			opt4.set_Checked(Settings.Default.CSVPaste4);
			((Control)opt4).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CSVPaste4", true, (DataSourceUpdateMode)1));
			((Control)opt4).set_Location(new Point(117, 46));
			((Control)opt4).set_Name("opt4");
			((Control)opt4).set_Size(new Size(65, 17));
			((Control)opt4).set_TabIndex(2);
			((Control)opt4).set_Text("Object 4");
			((ButtonBase)opt4).set_UseVisualStyleBackColor(true);
			((Control)opt2).set_AutoSize(true);
			opt2.set_Checked(Settings.Default.CSVPaste2);
			((Control)opt2).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CSVPaste2", true, (DataSourceUpdateMode)1));
			((Control)opt2).set_Location(new Point(117, 24));
			((Control)opt2).set_Name("opt2");
			((Control)opt2).set_Size(new Size(65, 17));
			((Control)opt2).set_TabIndex(1);
			((Control)opt2).set_Text("Object 2");
			((ButtonBase)opt2).set_UseVisualStyleBackColor(true);
			((Control)opt1).set_AutoSize(true);
			opt1.set_Checked(Settings.Default.CSVPaste1);
			((Control)opt1).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CSVPaste1", true, (DataSourceUpdateMode)1));
			((Control)opt1).set_Location(new Point(20, 24));
			((Control)opt1).set_Name("opt1");
			((Control)opt1).set_Size(new Size(65, 17));
			((Control)opt1).set_TabIndex(0);
			opt1.set_TabStop(true);
			((Control)opt1).set_Text("Object 1");
			((ButtonBase)opt1).set_UseVisualStyleBackColor(true);
			((Control)optTangra).set_AutoSize(true);
			optTangra.set_Checked(Settings.Default.CSVPasteTangra);
			((Control)optTangra).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CSVPasteTangra", true, (DataSourceUpdateMode)1));
			((Control)optTangra).set_Location(new Point(117, 20));
			((Control)optTangra).set_Name("optTangra");
			((Control)optTangra).set_Size(new Size(59, 17));
			((Control)optTangra).set_TabIndex(1);
			((Control)optTangra).set_Text("Tangra");
			((ButtonBase)optTangra).set_UseVisualStyleBackColor(true);
			((Control)optLiMovie).set_AutoSize(true);
			optLiMovie.set_Checked(Settings.Default.CSVPasteLiMovie);
			((Control)optLiMovie).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "CSVPasteLiMovie", true, (DataSourceUpdateMode)1));
			((Control)optLiMovie).set_Location(new Point(20, 22));
			((Control)optLiMovie).set_Name("optLiMovie");
			((Control)optLiMovie).set_Size(new Size(62, 17));
			((Control)optLiMovie).set_TabIndex(0);
			optLiMovie.set_TabStop(true);
			((Control)optLiMovie).set_Text("LiMovie");
			((ButtonBase)optLiMovie).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(222, 195));
			((Control)this).get_Controls().Add((Control)(object)cndCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("StarDiameterPaste");
			((Control)this).set_Text("Set CSV details");
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)this).ResumeLayout(false);
		}
	}
}
