using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Globalization;
using System.Text;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.File_Actions;
using Occult.Properties;

namespace Occult.Ephemerides
{
	public class AsteroidSatellites : Form
	{
		private List<BinaryAsteroidOffsets> BinaryOffsets;

		private IContainer components;

		private Label lblInternetWarning;

		private Label label4;

		private Label lblUpdateDate;

		private Label label11;

		private Button cmdMiriadeUpdateNow;

		private ListBox lstMiriadeAsteroids;

		private Button cmdGetMiriadeSolutions;

		private NumericUpDown updnYear;

		private NumericUpDown updnDay;

		private NumericUpDown updnMonth;

		private NumericUpDown updnMinute;

		private NumericUpDown updnHour;

		private Label label2;

		private Label label3;

		private Label label5;

		private Label label6;

		private Label label7;

		private Button cmdGenerate;

		private TextBox txtEphemeris;

		private Button cmdCopy;

		public AsteroidSatellites()
		{
			InitializeComponent();
		}

		private void AsteroidSatellites_Load(object sender, EventArgs e)
		{
			http.UpdateListOfMiriadeBinaries(MustUpdateNow: false);
			ShowListOfMiriadeAsteroids();
			((Control)lblInternetWarning).set_Enabled(Utilities.InternetIsAvailable());
			updnYear.set_Value((decimal)DateTime.UtcNow.Year);
			updnMonth.set_Value((decimal)DateTime.UtcNow.Month);
			updnDay.set_Value((decimal)DateTime.UtcNow.Day);
			updnHour.set_Value((decimal)DateTime.UtcNow.Hour);
			updnMinute.set_Value((decimal)DateTime.UtcNow.Minute);
		}

		private void ShowListOfMiriadeAsteroids()
		{
			string[] array = Settings.Default.MiriadeAsteroids.Split(new char[1] { ' ' });
			lstMiriadeAsteroids.get_Items().Clear();
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Length > 0)
				{
					lstMiriadeAsteroids.get_Items().Add((object)array[i]);
				}
			}
			((ListControl)lstMiriadeAsteroids).set_SelectedIndex(0);
			DateTime miriadeBinaryUpdateDate = Settings.Default.MiriadeBinaryUpdateDate;
			((Control)lblUpdateDate).set_Text(miriadeBinaryUpdateDate.Year + " " + CultureInfo.CurrentCulture.DateTimeFormat.GetMonthName(miriadeBinaryUpdateDate.Month) + " " + miriadeBinaryUpdateDate.Day);
		}

		private void cmdMiriadeUpdateNow_Click(object sender, EventArgs e)
		{
			ShowListOfMiriadeAsteroids();
			MinorPlanetOccultationElements.CreateBinaryAstroidsInMiriade();
		}

		private void cmdGetMiriadeSolutions_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new MiriadeSolutions()).ShowDialog();
		}

		private void cmdGenerate_Click(object sender, EventArgs e)
		{
			BinaryOffsets = new List<BinaryAsteroidOffsets>();
			double eventReferenceTime = Utilities.JD_from_Date((int)updnYear.get_Value(), (int)updnMonth.get_Value(), (double)updnDay.get_Value() + 1.0 / 24.0 + (double)updnHour.get_Value() / 24.0 + (double)updnMinute.get_Value() / 1440.0);
			XMLprocess.GetMiraideEphemeris(int.Parse(lstMiriadeAsteroids.get_Items().get_Item(((ListControl)lstMiriadeAsteroids).get_SelectedIndex()).ToString()), eventReferenceTime, ref BinaryOffsets, SaveMiriadeResponse: false);
			StringBuilder stringBuilder = new StringBuilder();
			for (int i = 0; i < BinaryOffsets.Count; i++)
			{
				stringBuilder.Append("Name: " + BinaryOffsets[i].ComponentName);
				stringBuilder.AppendFormat("   Dia {0} km", BinaryOffsets[i].ComponentDiameter);
				stringBuilder.Append("    Solution type: " + BinaryOffsets[i].SolutionType + "\r\n");
				for (int j = 0; j < 4; j++)
				{
					stringBuilder.Append(Utilities.DateTime_from_JD(BinaryOffsets[i].Offset_JD[j]));
					stringBuilder.AppendFormat(": dX = {0,7:f4} ±{1,5:f4}, dY = {2,7:f4} ±{3,5:f4}\r\n", BinaryOffsets[i].Offset_RA_mas[j] / 1000.0, BinaryOffsets[i].SigmaMajor_mas / 1000.0, BinaryOffsets[i].Offset_Dec_mas[j] / 1000.0, BinaryOffsets[i].SigmaMinor_mas / 1000.0);
				}
				stringBuilder.Append("\r\n");
			}
			((Control)txtEphemeris).set_Text(stringBuilder.ToString());
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(((Control)txtEphemeris).get_Text());
		}

		private void AsteroidSatellites_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(665);
			if (((Control)this).get_Height() < 200)
			{
				((Control)this).set_Height(200);
			}
			((Control)lstMiriadeAsteroids).set_Height(((Control)this).get_Height() - 139);
			((Control)txtEphemeris).set_Height(((Control)this).get_Height() - 181);
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
			//IL_007f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0089: Expected O, but got Unknown
			//IL_008a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0094: Expected O, but got Unknown
			//IL_0095: Unknown result type (might be due to invalid IL or missing references)
			//IL_009f: Expected O, but got Unknown
			//IL_00a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_00aa: Expected O, but got Unknown
			//IL_00ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b5: Expected O, but got Unknown
			//IL_00b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c0: Expected O, but got Unknown
			//IL_00c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cb: Expected O, but got Unknown
			//IL_00cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d6: Expected O, but got Unknown
			//IL_00d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e1: Expected O, but got Unknown
			//IL_00e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ec: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(AsteroidSatellites));
			lblInternetWarning = new Label();
			label4 = new Label();
			lblUpdateDate = new Label();
			label11 = new Label();
			cmdMiriadeUpdateNow = new Button();
			lstMiriadeAsteroids = new ListBox();
			cmdGetMiriadeSolutions = new Button();
			updnYear = new NumericUpDown();
			updnDay = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnMinute = new NumericUpDown();
			updnHour = new NumericUpDown();
			label2 = new Label();
			label3 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			cmdGenerate = new Button();
			txtEphemeris = new TextBox();
			cmdCopy = new Button();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnMinute).BeginInit();
			((ISupportInitialize)updnHour).BeginInit();
			((Control)this).SuspendLayout();
			((Control)lblInternetWarning).set_AutoSize(true);
			((Control)lblInternetWarning).set_BackColor(Color.MediumSpringGreen);
			((Control)lblInternetWarning).set_ForeColor(Color.Red);
			((Control)lblInternetWarning).set_Location(new Point(22, 58));
			((Control)lblInternetWarning).set_Name("lblInternetWarning");
			((Control)lblInternetWarning).set_Size(new Size(170, 26));
			((Control)lblInternetWarning).set_TabIndex(19);
			((Control)lblInternetWarning).set_Text("To get binary asteroid data from \r\nMiriade, you need Internet access!");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(18, 28));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(170, 26));
			((Control)label4).set_TabIndex(13);
			((Control)label4).set_Text("Binary asteroid ephemerides \r\navailable from Miriade");
			((Control)lblUpdateDate).set_AutoSize(true);
			((Control)lblUpdateDate).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUpdateDate).set_Location(new Point(107, 174));
			((Control)lblUpdateDate).set_Name("lblUpdateDate");
			((Control)lblUpdateDate).set_Size(new Size(53, 12));
			((Control)lblUpdateDate).set_TabIndex(23);
			((Control)lblUpdateDate).set_Text("Last update");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(107, 161));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(53, 12));
			((Control)label11).set_TabIndex(22);
			((Control)label11).set_Text("Last update");
			((Control)cmdMiriadeUpdateNow).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdMiriadeUpdateNow).set_Location(new Point(102, 139));
			((Control)cmdMiriadeUpdateNow).set_Name("cmdMiriadeUpdateNow");
			((Control)cmdMiriadeUpdateNow).set_Size(new Size(79, 20));
			((Control)cmdMiriadeUpdateNow).set_TabIndex(24);
			((Control)cmdMiriadeUpdateNow).set_Text("Update list");
			((ButtonBase)cmdMiriadeUpdateNow).set_UseVisualStyleBackColor(true);
			((Control)cmdMiriadeUpdateNow).add_Click((EventHandler)cmdMiriadeUpdateNow_Click);
			((ListControl)lstMiriadeAsteroids).set_FormattingEnabled(true);
			((Control)lstMiriadeAsteroids).set_Location(new Point(25, 96));
			((Control)lstMiriadeAsteroids).set_Name("lstMiriadeAsteroids");
			((Control)lstMiriadeAsteroids).set_RightToLeft((RightToLeft)1);
			lstMiriadeAsteroids.set_ScrollAlwaysVisible(true);
			((Control)lstMiriadeAsteroids).set_Size(new Size(71, 342));
			((Control)lstMiriadeAsteroids).set_TabIndex(0);
			((Control)cmdGetMiriadeSolutions).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGetMiriadeSolutions).set_Location(new Point(105, 224));
			((Control)cmdGetMiriadeSolutions).set_Name("cmdGetMiriadeSolutions");
			((Control)cmdGetMiriadeSolutions).set_Size(new Size(76, 35));
			((Control)cmdGetMiriadeSolutions).set_TabIndex(25);
			((Control)cmdGetMiriadeSolutions).set_Text("List solution details");
			((ButtonBase)cmdGetMiriadeSolutions).set_UseVisualStyleBackColor(true);
			((Control)cmdGetMiriadeSolutions).add_Click((EventHandler)cmdGetMiriadeSolutions_Click);
			((Control)updnYear).set_Location(new Point(228, 96));
			updnYear.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1900, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(48, 20));
			((Control)updnYear).set_TabIndex(27);
			updnYear.set_Value(new decimal(new int[4] { 2020, 0, 0, 0 }));
			((Control)updnDay).set_Location(new Point(331, 96));
			updnDay.set_Maximum(new decimal(new int[4] { 31, 0, 0, 0 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(38, 20));
			((Control)updnDay).set_TabIndex(28);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Location(new Point(282, 96));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(43, 20));
			((Control)updnMonth).set_TabIndex(29);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMinute).set_Location(new Point(419, 96));
			updnMinute.set_Maximum(new decimal(new int[4] { 59, 0, 0, 0 }));
			((Control)updnMinute).set_Name("updnMinute");
			((Control)updnMinute).set_Size(new Size(38, 20));
			((Control)updnMinute).set_TabIndex(30);
			((Control)updnHour).set_Location(new Point(375, 96));
			updnHour.set_Maximum(new decimal(new int[4] { 23, 0, 0, 0 }));
			((Control)updnHour).set_Name("updnHour");
			((Control)updnHour).set_Size(new Size(38, 20));
			((Control)updnHour).set_TabIndex(31);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(229, 80));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(29, 13));
			((Control)label2).set_TabIndex(32);
			((Control)label2).set_Text("Year");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(285, 80));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(37, 13));
			((Control)label3).set_TabIndex(33);
			((Control)label3).set_Text("Month");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(334, 80));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(26, 13));
			((Control)label5).set_TabIndex(34);
			((Control)label5).set_Text("Day");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(378, 80));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(30, 13));
			((Control)label6).set_TabIndex(35);
			((Control)label6).set_Text("Hour");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(422, 80));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(24, 13));
			((Control)label7).set_TabIndex(36);
			((Control)label7).set_Text("Min");
			((Control)cmdGenerate).set_Location(new Point(227, 31));
			((Control)cmdGenerate).set_Name("cmdGenerate");
			((Control)cmdGenerate).set_Size(new Size(228, 46));
			((Control)cmdGenerate).set_TabIndex(37);
			((Control)cmdGenerate).set_Text("Generate ephemeris of selected asteroid, at four 1-hour intervals starting at:");
			((ButtonBase)cmdGenerate).set_UseVisualStyleBackColor(true);
			((Control)cmdGenerate).add_Click((EventHandler)cmdGenerate_Click);
			((Control)txtEphemeris).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEphemeris).set_Location(new Point(225, 129));
			((TextBoxBase)txtEphemeris).set_Multiline(true);
			((Control)txtEphemeris).set_Name("txtEphemeris");
			txtEphemeris.set_ScrollBars((ScrollBars)2);
			((Control)txtEphemeris).set_Size(new Size(410, 308));
			((Control)txtEphemeris).set_TabIndex(38);
			((Control)cmdCopy).set_Location(new Point(521, 31));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(83, 46));
			((Control)cmdCopy).set_TabIndex(39);
			((Control)cmdCopy).set_Text("Copy ephemerides");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(true);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(649, 450));
			((Control)this).get_Controls().Add((Control)(object)cmdCopy);
			((Control)this).get_Controls().Add((Control)(object)txtEphemeris);
			((Control)this).get_Controls().Add((Control)(object)cmdGenerate);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)updnHour);
			((Control)this).get_Controls().Add((Control)(object)updnMinute);
			((Control)this).get_Controls().Add((Control)(object)updnMonth);
			((Control)this).get_Controls().Add((Control)(object)updnDay);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)cmdGetMiriadeSolutions);
			((Control)this).get_Controls().Add((Control)(object)cmdMiriadeUpdateNow);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)lblUpdateDate);
			((Control)this).get_Controls().Add((Control)(object)lblInternetWarning);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)lstMiriadeAsteroids);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Control)this).set_Name("AsteroidSatellites");
			((Control)this).set_Text("Generate ephemeris for binary asteroids");
			((Form)this).add_Load((EventHandler)AsteroidSatellites_Load);
			((Control)this).add_Resize((EventHandler)AsteroidSatellites_Resize);
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnMinute).EndInit();
			((ISupportInitialize)updnHour).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
