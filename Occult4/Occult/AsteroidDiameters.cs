using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class AsteroidDiameters : Form
	{
		private readonly string DiameterFile = Utilities.AppPath + "\\Resource Files\\AsteroidDiameters.bin";

		private int AsteroidNumber;

		private IContainer components;

		private Button cmdReplace;

		private Label label2;

		private Label label1;

		private TextBox txtMeanDia;

		private TextBox txtAsteroid;

		private Button cmdHelp;

		private Label label3;

		private Label label4;

		private TextBox txtUncert;

		private Label label5;

		public AsteroidDiameters()
		{
			InitializeComponent();
		}

		private void AsteroidDiameters_Load(object sender, EventArgs e)
		{
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			((Control)txtAsteroid).set_Text("1");
		}

		private void AsteroidDiameters_FormClosing(object sender, FormClosingEventArgs e)
		{
		}

		private void txtAsteroid_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtAsteroid).SelectAll();
		}

		private void txtMeanDia_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMeanDia).SelectAll();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid Diameters - Edit");
		}

		private void txtAsteroid_TextChanged(object sender, EventArgs e)
		{
			if (!int.TryParse(((Control)txtAsteroid).get_Text(), out AsteroidNumber))
			{
				AsteroidNumber = 0;
			}
			Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo(AsteroidNumber, out var Diameter, out var DiameterUncertainty, out var _);
			((Control)txtMeanDia).set_Text(string.Format("{0,1:f1}", Diameter));
			((Control)txtUncert).set_Text(string.Format("{0,1:f1}", DiameterUncertainty));
		}

		private void cmdReplace_Click(object sender, EventArgs e)
		{
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01af: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b5: Invalid comparison between Unknown and I4
			double Diameter = 0.0;
			double DiameterUncertainty = 0.0;
			int num = 660000;
			if ((AsteroidNumber > 0) & (AsteroidNumber <= num))
			{
				Utilities.Get_SingleAsteroid_Diameter_and_UncertaintyInfo(AsteroidNumber, out Diameter, out DiameterUncertainty, out var _);
				if (!double.TryParse(((Control)txtMeanDia).get_Text(), out var result) | !double.TryParse(((Control)txtUncert).get_Text(), out var result2))
				{
					MessageBox.Show("You have entered invalid text in either Mean Diameter or Uncertainty\r\n\r\nPlease ensure numeric values are entered", "Invalid data", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				if (result <= 0.0 || result > 3000.0)
				{
					MessageBox.Show("The value for the new diameter is greater than 3000 km.\r\n\r\nPlease ensure the diameter is greater than 0.0 and less than 3000", "Invalid data", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				if (result2 <= 0.0 || result2 > 0.1 * result)
				{
					MessageBox.Show("The value for the new uncertainty is greater than 10% of the new diameter.\r\n\r\nPlease ensure the uncertainty is less than 10%", "Invalid data", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				short value = (short)(result * 10.0);
				byte value2 = (byte)(result2 * 5.0);
				if ((int)MessageBox.Show("You are about to change the mean diameter for\r\nasteroid # " + AsteroidNumber + "\r\n\r\nfrom : " + string.Format("{0,1:f1} ± {1,1:f1} km", Diameter, DiameterUncertainty) + "\r\n\r\nto : " + string.Format("{0,1:f1} ± {1,1:f1} km", result, result2) + "\r\n\r\nDo you want to proceed with the replacement", "Confirm update", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 6)
				{
					FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource Files\\AsteroidDiameters.bin", FileMode.Open, FileAccess.Write);
					BinaryWriter binaryWriter = new BinaryWriter(fileStream);
					fileStream.Seek(2 * (AsteroidNumber - 1), SeekOrigin.Begin);
					binaryWriter.Write(value);
					fileStream.Seek(2 * num + AsteroidNumber - 1, SeekOrigin.Begin);
					binaryWriter.Write(value2);
					fileStream.Close();
				}
			}
			else
			{
				MessageBox.Show("The asteroid number must be numeric, and be greater than 0 and less than " + num + "\r\n\r\nPlease ensure the value entered is within this range", "Invalid data", (MessageBoxButtons)0, (MessageBoxIcon)16);
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
			//IL_005e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Expected O, but got Unknown
			//IL_0069: Unknown result type (might be due to invalid IL or missing references)
			//IL_0073: Expected O, but got Unknown
			//IL_0074: Unknown result type (might be due to invalid IL or missing references)
			//IL_007e: Expected O, but got Unknown
			//IL_05a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_05aa: Expected O, but got Unknown
			//IL_0605: Unknown result type (might be due to invalid IL or missing references)
			//IL_060f: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidDiameters));
			cmdHelp = new Button();
			cmdReplace = new Button();
			label2 = new Label();
			label1 = new Label();
			txtMeanDia = new TextBox();
			txtAsteroid = new TextBox();
			label3 = new Label();
			label4 = new Label();
			txtUncert = new TextBox();
			label5 = new Label();
			((Control)this).SuspendLayout();
			((Control)cmdHelp).set_Location(new Point(208, 232));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(41, 24));
			((Control)cmdHelp).set_TabIndex(13);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)cmdReplace).set_Location(new Point(78, 301));
			((Control)cmdReplace).set_Name("cmdReplace");
			((Control)cmdReplace).set_Size(new Size(110, 28));
			((Control)cmdReplace).set_TabIndex(8);
			((Control)cmdReplace).set_Text("Replace diameter");
			((ButtonBase)cmdReplace).set_UseVisualStyleBackColor(true);
			((Control)cmdReplace).add_Click((EventHandler)cmdReplace_Click);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(9, 266));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(77, 13));
			((Control)label2).set_TabIndex(2);
			((Control)label2).set_Text("Mean diameter");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(11, 238));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(75, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Minor planet #");
			((Control)txtMeanDia).set_Location(new Point(88, 262));
			((Control)txtMeanDia).set_Name("txtMeanDia");
			((Control)txtMeanDia).set_Size(new Size(45, 20));
			((Control)txtMeanDia).set_TabIndex(3);
			((Control)txtMeanDia).add_Enter((EventHandler)txtMeanDia_Enter);
			((Control)txtAsteroid).set_Location(new Point(88, 234));
			((Control)txtAsteroid).set_Name("txtAsteroid");
			((Control)txtAsteroid).set_Size(new Size(45, 20));
			((Control)txtAsteroid).set_TabIndex(1);
			((Control)txtAsteroid).add_TextChanged((EventHandler)txtAsteroid_TextChanged);
			((Control)txtAsteroid).add_Enter((EventHandler)txtAsteroid_Enter);
			((Control)label3).set_AutoSize(true);
			label3.set_BorderStyle((BorderStyle)1);
			((Control)label3).set_Location(new Point(5, 10));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(256, 210));
			((Control)label3).set_TabIndex(14);
			((Control)label3).set_Text(componentResourceManager.GetString("label3.Text"));
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(189, 266));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(21, 13));
			((Control)label4).set_TabIndex(15);
			((Control)label4).set_Text("km");
			((Control)txtUncert).set_Location(new Point(154, 262));
			((Control)txtUncert).set_Name("txtUncert");
			((Control)txtUncert).set_Size(new Size(30, 20));
			((Control)txtUncert).set_TabIndex(16);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(135, 262));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(18, 20));
			((Control)label5).set_TabIndex(17);
			((Control)label5).set_Text("±");
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)0);
			((Form)this).set_ClientSize(new Size(266, 336));
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)txtUncert);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)txtAsteroid);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)txtMeanDia);
			((Control)this).get_Controls().Add((Control)(object)cmdReplace);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationAsterDias", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)3);
			((Form)this).set_Location(Settings.Default.LocationAsterDias);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("AsteroidDiameters");
			((Control)this).set_Text("Edit file of asteroid diameters");
			((Form)this).add_FormClosing(new FormClosingEventHandler(AsteroidDiameters_FormClosing));
			((Form)this).add_Load((EventHandler)AsteroidDiameters_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
