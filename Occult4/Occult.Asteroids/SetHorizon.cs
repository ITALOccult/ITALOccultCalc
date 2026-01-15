using System;
using System.ComponentModel;
using System.Configuration;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroids
{
	public class SetHorizon : Form
	{
		internal NumericUpDown[] Azimuth = (NumericUpDown[])(object)new NumericUpDown[24];

		internal Label[] Range = (Label[])(object)new Label[24];

		private int[] Altitudes = new int[24];

		private string InitialAltitudeString = Settings.Default.LocalHorizon;

		private IContainer components;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Button button1;

		private Button cmdUndo;

		private Button cmdCancel;

		private Label label13;

		private Button cmdClearAll;

		public SetHorizon()
		{
			//IL_008d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0093: Expected O, but got Unknown
			//IL_01c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01cc: Expected O, but got Unknown
			InitializeComponent();
			string[] array = InitialAltitudeString.Split(new char[1] { ',' });
			for (int i = 0; i < 24; i++)
			{
				int num = i * 15;
				int num2 = (i + 1) * 15;
				int num3 = 140 + 80 * (i % 6);
				int num4 = 47 + 60 * (i / 6);
				Azimuth[i] = new NumericUpDown();
				((Control)Azimuth[i]).set_Top(num4);
				((Control)Azimuth[i]).set_Left(num3);
				((Control)Azimuth[i]).set_Width(40);
				((Control)Azimuth[i]).set_Text(num + "°-" + num2 + "°");
				((UpDownBase)Azimuth[i]).set_TextAlign((HorizontalAlignment)1);
				((Control)Azimuth[i]).set_Font(new Font("Arial", 9f, FontStyle.Bold));
				Azimuth[i].set_Minimum(0m);
				Azimuth[i].set_Maximum(90m);
				Azimuth[i].set_Value((decimal)int.Parse(array[i]));
				Azimuth[i].set_DecimalPlaces(0);
				Azimuth[i].set_Increment(1m);
				((Control)Azimuth[i]).set_Visible(true);
				((Control)Azimuth[i]).set_Enabled(true);
				((Control)this).get_Controls().Add((Control)(object)Azimuth[i]);
				((Control)Azimuth[i]).BringToFront();
				((Control)Azimuth[i]).set_TabIndex(i);
				Range[i] = new Label();
				((Control)Range[i]).set_Text(num + "°-" + num2 + "°");
				((Control)Range[i]).set_Top(num4 - ((Control)Azimuth[i]).get_Height() + 5);
				((Control)Range[i]).set_AutoSize(true);
				((Control)this).get_Controls().Add((Control)(object)Range[i]);
				((Control)Range[i]).set_Left(num3 + ((Control)Azimuth[i]).get_Width() / 2 - ((Control)Range[i]).get_Width() / 2);
			}
		}

		private void button1_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you  sure you want to Save and Exit?", "Confirm Save & Exit", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string text = ((int)Azimuth[0].get_Value()).ToString();
				for (int i = 1; i < 24; i++)
				{
					text = text + "," + (int)Azimuth[i].get_Value();
				}
				Settings.Default.LocalHorizon = text;
				((SettingsBase)Settings.Default).Save();
				((Form)this).Close();
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you  sure you want to Exit and lose any changes?", "Confirm Cancel", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((Form)this).Close();
			}
		}

		private void cmdUndo_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you  sure you want to Undo all changes?", "Confirm Undo", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				string[] array = InitialAltitudeString.Split(new char[1] { ',' });
				for (int i = 0; i < 24; i++)
				{
					Azimuth[i].set_Value((decimal)int.Parse(array[i]));
				}
			}
		}

		private void cmdClearAll_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to set all elevations to 0°?", "Confirm set to zero", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				for (int i = 0; i < 24; i++)
				{
					Azimuth[i].set_Value(0m);
				}
			}
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
			//IL_009b: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Expected O, but got Unknown
			//IL_00a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b0: Expected O, but got Unknown
			//IL_00b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00bb: Expected O, but got Unknown
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			button1 = new Button();
			cmdUndo = new Button();
			cmdCancel = new Button();
			label13 = new Label();
			cmdClearAll = new Button();
			((Control)this).SuspendLayout();
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(85, 30));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(44, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Azimuth");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(87, 49));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(42, 13));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Altitude");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(87, 109));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(42, 13));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("Altitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(85, 90));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(44, 13));
			((Control)label4).set_TabIndex(2);
			((Control)label4).set_Text("Azimuth");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(87, 169));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(42, 13));
			((Control)label5).set_TabIndex(5);
			((Control)label5).set_Text("Altitude");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(85, 150));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(44, 13));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("Azimuth");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(87, 229));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(42, 13));
			((Control)label7).set_TabIndex(7);
			((Control)label7).set_Text("Altitude");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(85, 210));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(44, 13));
			((Control)label8).set_TabIndex(6);
			((Control)label8).set_Text("Azimuth");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(9, 40));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(75, 13));
			((Control)label9).set_TabIndex(8);
			((Control)label9).set_Text("North - East");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(7, 100));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(77, 13));
			((Control)label10).set_TabIndex(9);
			((Control)label10).set_Text("East - South");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(3, 160));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(81, 13));
			((Control)label11).set_TabIndex(10);
			((Control)label11).set_Text("South - West");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(5, 220));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(79, 13));
			((Control)label12).set_TabIndex(11);
			((Control)label12).set_Text("West - North");
			((Control)button1).set_Location(new Point(82, 270));
			((Control)button1).set_Name("button1");
			((Control)button1).set_Size(new Size(95, 36));
			((Control)button1).set_TabIndex(12);
			((Control)button1).set_Text("Save && exit");
			((ButtonBase)button1).set_UseVisualStyleBackColor(true);
			((Control)button1).add_Click((EventHandler)button1_Click);
			((Control)cmdUndo).set_Location(new Point(212, 270));
			((Control)cmdUndo).set_Name("cmdUndo");
			((Control)cmdUndo).set_Size(new Size(95, 36));
			((Control)cmdUndo).set_TabIndex(13);
			((Control)cmdUndo).set_Text("Undo all changes");
			((ButtonBase)cmdUndo).set_UseVisualStyleBackColor(true);
			((Control)cmdUndo).add_Click((EventHandler)cmdUndo_Click);
			((Control)cmdCancel).set_Location(new Point(472, 270));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(95, 36));
			((Control)cmdCancel).set_TabIndex(14);
			((Control)cmdCancel).set_Text("Exit with no changes");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(138, 4));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(373, 17));
			((Control)label13).set_TabIndex(15);
			((Control)label13).set_Text("Set elevation of horizon for each range in Azimuth");
			((Control)cmdClearAll).set_Location(new Point(342, 270));
			((Control)cmdClearAll).set_Name("cmdClearAll");
			((Control)cmdClearAll).set_Size(new Size(95, 36));
			((Control)cmdClearAll).set_TabIndex(16);
			((Control)cmdClearAll).set_Text("Set all elevations to zero");
			((ButtonBase)cmdClearAll).set_UseVisualStyleBackColor(true);
			((Control)cmdClearAll).add_Click((EventHandler)cmdClearAll_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(648, 320));
			((Control)this).get_Controls().Add((Control)(object)cmdClearAll);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdUndo);
			((Control)this).get_Controls().Add((Control)(object)button1);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Control)this).set_Name("SetHorizon");
			((Control)this).set_Text("Set Elevation of horizon");
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
