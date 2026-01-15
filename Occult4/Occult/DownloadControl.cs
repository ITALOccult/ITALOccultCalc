using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using Occult.File_Actions;
using Occult.Properties;

namespace Occult
{
	public class DownloadControl : Form
	{
		private IContainer components;

		private RadioButton optArchive_Exising;

		private RadioButton optArchive_New;

		private Label label1;

		private Label label2;

		private Panel panel1;

		private Panel panel2;

		private RadioButton optTycho2_New;

		private RadioButton optTycho2_Exising;

		private Panel panel3;

		private RadioButton optXZ_New;

		private RadioButton optXZ_Exising;

		private Panel panel4;

		private RadioButton optJPL_New;

		private RadioButton optJPL_Exising;

		private Panel panel6;

		private RadioButton optVersion_New;

		private RadioButton optVersion_Exising;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		private Button cmdWrite;

		private Label label10;

		private Panel panel5;

		private RadioButton optAddress_New;

		private RadioButton optAddress_Exising;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private Label label11;

		private Panel panel7;

		private RadioButton optAsteroid_New;

		private RadioButton optAsteroid_Exising;

		private Label label12;

		private Panel panel8;

		private RadioButton optLunar_New;

		private RadioButton optLunar_Exising;

		private Label label13;

		private Label label14;

		private Panel panel9;

		private RadioButton optdeltaT_New;

		private RadioButton optdeltaT_Exising;

		private Label label15;

		private Panel panel10;

		private RadioButton optBinary_New;

		private RadioButton optBinary_Exising;

		private Label label16;

		private Panel panel11;

		private RadioButton optDia_New;

		private RadioButton optDia_Exising;

		private PictureBox picAsteroid;

		private PictureBox picArchive;

		private PictureBox picTycho2;

		private PictureBox picXZ;

		private PictureBox picJPLDE;

		private PictureBox picAddresses;

		private PictureBox picRecent;

		private PictureBox picdT;

		private PictureBox picBinary;

		private PictureBox picDia;

		private Label label19;

		private Label label20;

		private Panel panel14;

		private PictureBox picStarDia;

		private RadioButton optStarDia_New;

		private RadioButton optStarDia_Exising;

		private Label label21;

		private Panel panel15;

		private RadioButton optBeta_New;

		private RadioButton optBeta_Exising;

		private Label label22;

		private Panel panel16;

		private PictureBox picCameraDelays;

		private RadioButton optCameraDelays_New;

		private RadioButton optCameraDelays_Exising;

		private Label label23;

		private Panel panel17;

		private PictureBox picRings;

		private RadioButton optRings_New;

		private RadioButton optRings_Exising;

		private Label label24;

		private Panel panel18;

		private PictureBox picK2;

		private RadioButton optK2_New;

		private RadioButton optK2_Exising;

		private Label label25;

		private Panel panel19;

		private PictureBox picLOLA;

		private RadioButton optLOLA_New;

		private RadioButton optLOLA_Exising;

		private Label label26;

		private Panel panel20;

		private PictureBox picLC;

		private RadioButton optLC_New;

		private RadioButton optLC_Exising;

		private Label label28;

		private Panel panel21;

		private PictureBox picISAM;

		private RadioButton optISAM_New;

		private RadioButton optISAM_Exising;

		private Label label29;

		private Panel panel22;

		private PictureBox picGaia14;

		private RadioButton optGaia14_New;

		private RadioButton optGaia14_Existing;

		private Label label30;

		private Panel panel23;

		private PictureBox picGaia12;

		private RadioButton optGaia12_New;

		private RadioButton optGaia12_Existing;

		private Label label31;

		private Panel panel24;

		private PictureBox picClass;

		private RadioButton optClass_New;

		private RadioButton optClass_Exising;

		private Label label32;

		private Panel panel25;

		private PictureBox picShapes;

		private RadioButton optShapes_New;

		private RadioButton optShapes_Exising;

		private Label label27;

		private Panel panel26;

		private PictureBox picGaia16;

		private RadioButton optGaia16_New;

		private RadioButton optGaia16_Existing;

		private Label label17;

		private Label label18;

		private Panel panel12;

		private PictureBox picGaia9;

		private RadioButton optGaia9_New;

		private RadioButton optGaia9_Existing;

		private Label label33;

		private Panel panel13;

		private PictureBox picAAVSO;

		private RadioButton optAAVSO_new;

		private RadioButton optAAVSO_existing;

		public DownloadControl()
		{
			InitializeComponent();
		}

		private void DownloadControl_Load(object sender, EventArgs e)
		{
			//IL_09db: Unknown result type (might be due to invalid IL or missing references)
			string path = Utilities.AppPath + "\\Resource Files\\asteroid_observations.zip";
			string path2 = Utilities.AppPath + "\\Resource Files\\" + Utilities.AsteroidObservationsFile;
			string path3 = Utilities.AppPath + "\\Resource Files\\latest_observations.zip";
			string path4 = Utilities.AppPath + "\\Resource Files\\Archive Observations recent.dat";
			string path5 = Utilities.AppPath + "\\LightCurves\\LightCurves.zip";
			string path6 = Utilities.AppPath + "\\LightCurves\\LightCurves.txt";
			string path7 = Utilities.AppPath + "\\Resource Files\\kepler2.zip";
			string path8 = Utilities.AppPath + "\\Resource Files\\Kepler2.dat";
			string path9 = Utilities.AppPath + "\\Resource Files\\binaryasteroids.zip";
			string path10 = Utilities.AppPath + "\\Resource Files\\BinaryAsteroids.csv";
			string path11 = Utilities.AppPath + "\\Resource Files\\asteroidclasses.zip";
			string path12 = Utilities.AppPath + "\\Resource Files\\asteroidclasses.csv";
			string path13 = Utilities.AppPath + "\\Resource Files\\XZ.zip";
			string path14 = Utilities.AppPath + "\\Resource Files\\xz80.dat";
			string path15 = Utilities.AppPath + "\\Resource Files\\asteroidrings.zip";
			string path16 = Utilities.AppPath + "\\Resource Files\\AsteroidRings.csv";
			string path17 = Utilities.AppPath + "\\Resource Files\\deltat.zip";
			string path18 = Utilities.AppPath + "\\Resource Files\\DeltaTA.dat";
			string path19 = Utilities.AppPath + "\\Resource Files\\addresses.zip";
			string path20 = Utilities.AppPath + "\\Resource Files\\addresses.txt";
			string path21 = Utilities.AppPath + "\\Resource Files\\CameraDelays.zip";
			string path22 = Utilities.AppPath + "\\Resource Files\\CameraDelays.dat";
			string path23 = Utilities.AppPath + "\\Resource Files\\archive_observations.zip";
			string path24 = Utilities.AppPath + "\\Resource Files\\RArchive Observations 2016_2025.dat";
			string path25 = Utilities.AppPath + "\\Resource Files\\LOLA128.zip";
			string path26 = Utilities.AppPath + "\\Resource Files\\Lola128.bin";
			string path27 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia9_EDR3.zip";
			string path28 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia9_EDR3.bin";
			string path29 = Utilities.AppPath + "\\Resource Files\\Tycho2.zip";
			string path30 = Utilities.AppPath + "\\Resource Files\\Tycho2.bin";
			string path31 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia12_EDR3.zip";
			string path32 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia12_EDR3.bin";
			string path33 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia14_EDR3.zip";
			string path34 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia14_EDR3.bin";
			string path35 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia16_EDR3.zip";
			string path36 = Utilities.AppPath + "\\Resource Files\\Gaia\\Gaia16_EDR3.bin";
			string path37 = Utilities.AppPath + "\\Resource Files\\de_ephemeris.zip";
			string path38 = Utilities.AppPath + "\\Resource Files\\DE_Ephemeris.bin";
			string path39 = Utilities.AppPath + "\\Resource Files\\asteroiddiameters.zip";
			string path40 = Utilities.AppPath + "\\Resource Files\\AsteroidDiameters.bin";
			string path41 = Utilities.AppPath + "\\ShapeModels\\ISAM_availableasteroids.zip";
			string path42 = Utilities.AppPath + "\\ShapeModels\\ISAM_availableasteroids.csv";
			string path43 = Utilities.AppPath + "\\Resource Files\\StarDia.zip";
			string path44 = Utilities.AppPath + "\\Resource Files\\StarDia.bin";
			string path45 = Utilities.AppPath + "\\ShapeModels\\shapemodels.zip";
			string path46 = Utilities.AppPath + "\\ShapeModels\\DamitModels.csv";
			string path47 = Utilities.AppPath + "\\Downloaded Files\\AAVSOindex.zip";
			string path48 = Utilities.AppPath + "\\Downloaded Files\\AAVSOindex.dat";
			((Control)picAsteroid).set_Visible(!File.Exists(path));
			((Control)picRecent).set_Visible(!File.Exists(path3));
			((Control)picAddresses).set_Visible(!File.Exists(path19));
			((Control)picdT).set_Visible(!File.Exists(path17));
			((Control)picBinary).set_Visible(!File.Exists(path9));
			((Control)picRings).set_Visible(!File.Exists(path15));
			((Control)picDia).set_Visible(!File.Exists(path39));
			((Control)picClass).set_Visible(!File.Exists(path11));
			((Control)picArchive).set_Visible(!File.Exists(path23));
			((Control)picXZ).set_Visible(!File.Exists(path13));
			((Control)picLC).set_Visible(!File.Exists(path5));
			((Control)picTycho2).set_Visible(!File.Exists(path29));
			((Control)picGaia9).set_Visible(!File.Exists(path27));
			((Control)picGaia12).set_Visible(!File.Exists(path31));
			((Control)picGaia14).set_Visible(!File.Exists(path33));
			((Control)picGaia16).set_Visible(!File.Exists(path35));
			((Control)picJPLDE).set_Visible(!File.Exists(path37));
			((Control)picLOLA).set_Visible(!File.Exists(path25));
			((Control)picStarDia).set_Visible(!File.Exists(path43));
			((Control)picCameraDelays).set_Visible(!File.Exists(path21));
			((Control)picK2).set_Visible(!File.Exists(path7));
			((Control)picISAM).set_Visible(!File.Exists(path41));
			((Control)picShapes).set_Visible(!File.Exists(path45));
			((Control)picAAVSO).set_Visible(!File.Exists(path47));
			if (http.DownloadHTTP(Utilities.OccultServer(), "DownloadControl.txt", Downloads.DownloadContolFile, unzip: false, gunzip: false, ShowMessages: false, ShowProgressbar: false))
			{
				using (StreamReader streamReader = new StreamReader(Downloads.DownloadContolFile))
				{
					while (!streamReader.EndOfStream)
					{
						string text = streamReader.ReadLine();
						if (text.Substring(0, 8) == "<Occult>")
						{
							((Control)optVersion_Exising).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 10) == "OccultBeta")
						{
							((Control)optBeta_Exising).set_Text(text.Substring(12));
						}
						else if (text.Substring(1, 8) == "Asteroid")
						{
							((Control)optAsteroid_Exising).set_Text(text.Substring(10));
						}
						else if (text.Substring(1, 5) == "Lunar")
						{
							((Control)optLunar_Exising).set_Text(text.Substring(7));
						}
						else if (text.Substring(1, 9) == "Addresses")
						{
							((Control)optAddress_Exising).set_Text(text.Substring(11));
						}
						else if (text.Substring(1, 6) == "deltaT")
						{
							((Control)optdeltaT_Exising).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 6) == "Binary")
						{
							((Control)optBinary_Exising).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 5) == "Rings")
						{
							((Control)optRings_Exising).set_Text(text.Substring(7));
						}
						else if (text.Substring(1, 9) == "Diameters")
						{
							((Control)optDia_Exising).set_Text(text.Substring(11));
						}
						else if (text.Substring(1, 7) == "Classes")
						{
							((Control)optClass_Exising).set_Text(text.Substring(9));
						}
						else if (text.Substring(1, 7) == "Archive")
						{
							((Control)optArchive_Exising).set_Text(text.Substring(9));
						}
						else if (text.Substring(1, 2) == "XZ")
						{
							((Control)optXZ_Exising).set_Text(text.Substring(4));
						}
						else if (text.Substring(1, 6) == "Tycho2")
						{
							((Control)optTycho2_Exising).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 6) == "JPL_DE")
						{
							((Control)optJPL_Exising).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 7) == "Lola128")
						{
							((Control)optLOLA_Exising).set_Text(text.Substring(9));
						}
						else if (text.Substring(1, 7) == "StarDia")
						{
							((Control)optStarDia_Exising).set_Text(text.Substring(9));
						}
						else if (text.Substring(1, 12) == "CameraDelays")
						{
							((Control)optCameraDelays_Exising).set_Text(text.Substring(14));
						}
						else if (text.Substring(1, 7) == "Kepler2")
						{
							((Control)optK2_Exising).set_Text(text.Substring(9));
						}
						else if (text.Substring(1, 6) == "LCurve")
						{
							((Control)optLC_Exising).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 4) == "ISAM")
						{
							((Control)optISAM_Exising).set_Text(text.Substring(6));
						}
						else if (text.Substring(1, 5) == "Gaia9")
						{
							((Control)optGaia9_Existing).set_Text(text.Substring(7));
						}
						else if (text.Substring(1, 6) == "Gaia12")
						{
							((Control)optGaia12_Existing).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 6) == "Gaia14")
						{
							((Control)optGaia14_Existing).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 6) == "Gaia16")
						{
							((Control)optGaia16_Existing).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 6) == "Shapes")
						{
							((Control)optShapes_Exising).set_Text(text.Substring(8));
						}
						else if (text.Substring(1, 5) == "AAVSO")
						{
							((Control)optAAVSO_existing).set_Text(text.Substring(7));
						}
					}
				}
				DateTime time = GetTime(((Control)optAsteroid_Exising).get_Text());
				DateTime time2 = GetTime(((Control)optLunar_Exising).get_Text());
				DateTime time3 = GetTime(((Control)optAddress_Exising).get_Text());
				DateTime time4 = GetTime(((Control)optdeltaT_Exising).get_Text());
				DateTime time5 = GetTime(((Control)optBinary_Exising).get_Text());
				DateTime time6 = GetTime(((Control)optRings_Exising).get_Text());
				DateTime time7 = GetTime(((Control)optDia_Exising).get_Text());
				DateTime time8 = GetTime(((Control)optClass_Exising).get_Text());
				DateTime time9 = GetTime(((Control)optArchive_Exising).get_Text());
				DateTime time10 = GetTime(((Control)optXZ_Exising).get_Text());
				DateTime time11 = GetTime(((Control)optTycho2_Exising).get_Text());
				DateTime time12 = GetTime(((Control)optJPL_Exising).get_Text());
				DateTime time13 = GetTime(((Control)optLOLA_Exising).get_Text());
				DateTime time14 = GetTime(((Control)optStarDia_Exising).get_Text());
				DateTime time15 = GetTime(((Control)optCameraDelays_Exising).get_Text());
				DateTime time16 = GetTime(((Control)optK2_Exising).get_Text());
				DateTime time17 = GetTime(((Control)optLC_Exising).get_Text());
				DateTime time18 = GetTime(((Control)optISAM_Exising).get_Text());
				DateTime time19 = GetTime(((Control)optGaia9_Existing).get_Text());
				DateTime time20 = GetTime(((Control)optGaia12_Existing).get_Text());
				DateTime time21 = GetTime(((Control)optGaia14_Existing).get_Text());
				DateTime time22 = GetTime(((Control)optGaia16_Existing).get_Text());
				DateTime time23 = GetTime(((Control)optShapes_Exising).get_Text());
				DateTime time24 = GetTime(((Control)optAAVSO_existing).get_Text());
				DateTime date = File.GetLastWriteTime(path2).ToUniversalTime().Date;
				DateTime date2 = File.GetLastWriteTime(path4).ToUniversalTime().Date;
				DateTime date3 = File.GetLastWriteTime(path20).ToUniversalTime().Date;
				DateTime date4 = File.GetLastWriteTime(path18).ToUniversalTime().Date;
				DateTime date5 = File.GetLastWriteTime(path10).ToUniversalTime().Date;
				DateTime date6 = File.GetLastWriteTime(path16).ToUniversalTime().Date;
				DateTime date7 = File.GetLastWriteTime(path40).ToUniversalTime().Date;
				DateTime date8 = File.GetLastWriteTime(path12).ToUniversalTime().Date;
				DateTime date9 = File.GetLastWriteTime(path24).ToUniversalTime().Date;
				DateTime date10 = File.GetLastWriteTime(path14).ToUniversalTime().Date;
				DateTime date11 = File.GetLastWriteTime(path30).ToUniversalTime().Date;
				DateTime date12 = File.GetLastWriteTime(path38).ToUniversalTime().Date;
				DateTime date13 = File.GetLastWriteTime(path26).ToUniversalTime().Date;
				DateTime date14 = File.GetLastWriteTime(path44).ToUniversalTime().Date;
				DateTime date15 = File.GetLastWriteTime(path22).ToUniversalTime().Date;
				DateTime date16 = File.GetLastWriteTime(path8).ToUniversalTime().Date;
				DateTime date17 = File.GetLastWriteTime(path6).ToUniversalTime().Date;
				DateTime date18 = File.GetLastWriteTime(path42).ToUniversalTime().Date;
				DateTime date19 = File.GetLastWriteTime(path28).ToUniversalTime().Date;
				DateTime date20 = File.GetLastWriteTime(path32).ToUniversalTime().Date;
				DateTime date21 = File.GetLastWriteTime(path34).ToUniversalTime().Date;
				DateTime date22 = File.GetLastWriteTime(path36).ToUniversalTime().Date;
				DateTime date23 = File.GetLastWriteTime(path46).ToUniversalTime().Date;
				DateTime date24 = File.GetLastWriteTime(path48).ToUniversalTime().Date;
				RadioButton obj = optVersion_New;
				string text2;
				((Control)optBeta_New).set_Text(text2 = Assembly.GetExecutingAssembly().GetName(copiedName: false).Version!.ToString());
				((Control)obj).set_Text(text2);
				((Control)optAsteroid_New).set_Text(date.Day.ToString().PadLeft(2, '0') + date.Month.ToString().PadLeft(2, '0') + date.Year);
				((Control)optLunar_New).set_Text(date2.Day.ToString().PadLeft(2, '0') + date2.Month.ToString().PadLeft(2, '0') + date2.Year);
				((Control)optAddress_New).set_Text(date3.Day.ToString().PadLeft(2, '0') + date3.Month.ToString().PadLeft(2, '0') + date3.Year);
				((Control)optdeltaT_New).set_Text(date4.Day.ToString().PadLeft(2, '0') + date4.Month.ToString().PadLeft(2, '0') + date4.Year);
				((Control)optBinary_New).set_Text(date5.Day.ToString().PadLeft(2, '0') + date5.Month.ToString().PadLeft(2, '0') + date5.Year);
				((Control)optRings_New).set_Text(date6.Day.ToString().PadLeft(2, '0') + date6.Month.ToString().PadLeft(2, '0') + date6.Year);
				((Control)optDia_New).set_Text(date7.Day.ToString().PadLeft(2, '0') + date7.Month.ToString().PadLeft(2, '0') + date7.Year);
				((Control)optClass_New).set_Text(date8.Day.ToString().PadLeft(2, '0') + date8.Month.ToString().PadLeft(2, '0') + date8.Year);
				((Control)optArchive_New).set_Text(date9.Day.ToString().PadLeft(2, '0') + date9.Month.ToString().PadLeft(2, '0') + date9.Year);
				((Control)optXZ_New).set_Text(date10.Day.ToString().PadLeft(2, '0') + date10.Month.ToString().PadLeft(2, '0') + date10.Year);
				((Control)optTycho2_New).set_Text(date11.Day.ToString().PadLeft(2, '0') + date11.Month.ToString().PadLeft(2, '0') + date11.Year);
				((Control)optJPL_New).set_Text(date12.Day.ToString().PadLeft(2, '0') + date12.Month.ToString().PadLeft(2, '0') + date12.Year);
				((Control)optLOLA_New).set_Text(date13.Day.ToString().PadLeft(2, '0') + date13.Month.ToString().PadLeft(2, '0') + date13.Year);
				((Control)optStarDia_New).set_Text(date14.Day.ToString().PadLeft(2, '0') + date14.Month.ToString().PadLeft(2, '0') + date14.Year);
				((Control)optCameraDelays_New).set_Text(date15.Day.ToString().PadLeft(2, '0') + date15.Month.ToString().PadLeft(2, '0') + date15.Year);
				((Control)optK2_New).set_Text(date16.Day.ToString().PadLeft(2, '0') + date16.Month.ToString().PadLeft(2, '0') + date16.Year);
				((Control)optLC_New).set_Text(date17.Day.ToString().PadLeft(2, '0') + date17.Month.ToString().PadLeft(2, '0') + date17.Year);
				((Control)optISAM_New).set_Text(date18.Day.ToString().PadLeft(2, '0') + date18.Month.ToString().PadLeft(2, '0') + date18.Year);
				((Control)optGaia9_New).set_Text(date19.Day.ToString().PadLeft(2, '0') + date19.Month.ToString().PadLeft(2, '0') + date19.Year);
				((Control)optGaia12_New).set_Text(date20.Day.ToString().PadLeft(2, '0') + date20.Month.ToString().PadLeft(2, '0') + date20.Year);
				((Control)optGaia14_New).set_Text(date21.Day.ToString().PadLeft(2, '0') + date21.Month.ToString().PadLeft(2, '0') + date21.Year);
				((Control)optGaia16_New).set_Text(date22.Day.ToString().PadLeft(2, '0') + date22.Month.ToString().PadLeft(2, '0') + date22.Year);
				((Control)optShapes_New).set_Text(date23.Day.ToString().PadLeft(2, '0') + date23.Month.ToString().PadLeft(2, '0') + date23.Year);
				((Control)optAAVSO_new).set_Text(date24.Day.ToString().PadLeft(2, '0') + date24.Month.ToString().PadLeft(2, '0') + date24.Year);
				optVersion_New.set_Checked(false);
				optBeta_New.set_Checked(false);
				optAsteroid_New.set_Checked((time.CompareTo(date) < 0) & !((Control)picAsteroid).get_Visible());
				optLunar_New.set_Checked((time2.CompareTo(date2) < 0) & !((Control)picRecent).get_Visible());
				optAddress_New.set_Checked((time3.CompareTo(date3) < 0) & !((Control)picAddresses).get_Visible());
				optdeltaT_New.set_Checked((time4.CompareTo(date4) < 0) & !((Control)picdT).get_Visible());
				optBinary_New.set_Checked((time5.CompareTo(date5) < 0) & !((Control)picBinary).get_Visible());
				optRings_New.set_Checked((time6.CompareTo(date6) < 0) & !((Control)picRings).get_Visible());
				optArchive_New.set_Checked((time9.CompareTo(date9) < 0) & !((Control)picArchive).get_Visible());
				optDia_New.set_Checked((time7.CompareTo(date7) < 0) & !((Control)picDia).get_Visible());
				optClass_New.set_Checked((time8.CompareTo(date8) < 0) & !((Control)picClass).get_Visible());
				optXZ_New.set_Checked((time10.CompareTo(date10) < 0) & !((Control)picXZ).get_Visible());
				optTycho2_New.set_Checked((time11.CompareTo(date11) < 0) & !((Control)picTycho2).get_Visible());
				optJPL_New.set_Checked((time12.CompareTo(date12) < 0) & !((Control)picJPLDE).get_Visible());
				optLOLA_New.set_Checked((time13.CompareTo(date13) < 0) & !((Control)picLOLA).get_Visible());
				optStarDia_New.set_Checked((time14.CompareTo(date14) < 0) & !((Control)picStarDia).get_Visible());
				optCameraDelays_New.set_Checked((time15.CompareTo(date15) < 0) & !((Control)picCameraDelays).get_Visible());
				optK2_New.set_Checked((time16.CompareTo(date16) < 0) & !((Control)picK2).get_Visible());
				optLC_New.set_Checked((time17.CompareTo(date17) < 0) & !((Control)picLC).get_Visible());
				optISAM_New.set_Checked((time18.CompareTo(date18) < 0) & !((Control)picISAM).get_Visible());
				optGaia9_New.set_Checked((time19.CompareTo(date19) < 0) & !((Control)picGaia9).get_Visible());
				optGaia12_New.set_Checked((time20.CompareTo(date20) < 0) & !((Control)picGaia12).get_Visible());
				optGaia14_New.set_Checked((time21.CompareTo(date21) < 0) & !((Control)picGaia14).get_Visible());
				optGaia16_New.set_Checked((time22.CompareTo(date22) < 0) & !((Control)picGaia16).get_Visible());
				optShapes_New.set_Checked((time23.CompareTo(date23) < 0) & !((Control)picShapes).get_Visible());
				optAAVSO_new.set_Checked((time24.CompareTo(date23) < 0) & !((Control)picAAVSO).get_Visible());
				if (time.CompareTo(date) > 0)
				{
					((Control)optAsteroid_Exising).set_ForeColor(Color.Red);
				}
				if (time2.CompareTo(date2) > 0)
				{
					((Control)optLunar_Exising).set_ForeColor(Color.Red);
				}
				if (time3.CompareTo(date3) > 0)
				{
					((Control)optAddress_Exising).set_ForeColor(Color.Red);
				}
				if (time4.CompareTo(date4) > 0)
				{
					((Control)optdeltaT_Exising).set_ForeColor(Color.Red);
				}
				if (time5.CompareTo(date5) > 0)
				{
					((Control)optBinary_Exising).set_ForeColor(Color.Red);
				}
				if (time6.CompareTo(date6) > 0)
				{
					((Control)optRings_Exising).set_ForeColor(Color.Red);
				}
				if (time9.CompareTo(date9) > 0)
				{
					((Control)optArchive_Exising).set_ForeColor(Color.Red);
				}
				if (time7.CompareTo(date7) > 0)
				{
					((Control)optDia_Exising).set_ForeColor(Color.Red);
				}
				if (time8.CompareTo(date8) > 0)
				{
					((Control)optClass_Exising).set_ForeColor(Color.Red);
				}
				if (time10.CompareTo(date10) > 0)
				{
					((Control)optXZ_Exising).set_ForeColor(Color.Red);
				}
				if (time11.CompareTo(date11) > 0)
				{
					((Control)optTycho2_Exising).set_ForeColor(Color.Red);
				}
				if (time12.CompareTo(date12) > 0)
				{
					((Control)optJPL_Exising).set_ForeColor(Color.Red);
				}
				if (time13.CompareTo(date13) > 0)
				{
					((Control)optLOLA_Exising).set_ForeColor(Color.Red);
				}
				if (time14.CompareTo(date14) > 0)
				{
					((Control)optStarDia_Exising).set_ForeColor(Color.Red);
				}
				if (time15.CompareTo(date15) > 0)
				{
					((Control)optCameraDelays_Exising).set_ForeColor(Color.Red);
				}
				if (time16.CompareTo(date16) > 0)
				{
					((Control)optK2_Exising).set_ForeColor(Color.Red);
				}
				if (time17.CompareTo(date17) > 0)
				{
					((Control)optLC_Exising).set_ForeColor(Color.Red);
				}
				if (time18.CompareTo(date18) > 0)
				{
					((Control)optISAM_Exising).set_ForeColor(Color.Red);
				}
				if (time19.CompareTo(date19) > 0)
				{
					((Control)optGaia9_Existing).set_ForeColor(Color.Red);
				}
				if (time20.CompareTo(date20) > 0)
				{
					((Control)optGaia12_Existing).set_ForeColor(Color.Red);
				}
				if (time21.CompareTo(date21) > 0)
				{
					((Control)optGaia14_Existing).set_ForeColor(Color.Red);
				}
				if (time22.CompareTo(date22) > 0)
				{
					((Control)optGaia16_Existing).set_ForeColor(Color.Red);
				}
				if (time23.CompareTo(date23) > 0)
				{
					((Control)optShapes_Exising).set_ForeColor(Color.Red);
				}
				if (time24.CompareTo(date24) > 0)
				{
					((Control)optAAVSO_existing).set_ForeColor(Color.Red);
				}
			}
			else
			{
				MessageBox.Show("Existing downloadcontrol file has not been downloaded.\r\nProcess cannot continue");
			}
		}

		private DateTime GetTime(string T)
		{
			int day = int.Parse(T.Substring(0, 2));
			int month = int.Parse(T.Substring(2, 2));
			return new DateTime(int.Parse(T.Substring(4)), month, day);
		}

		private void cmdWrite_Click(object sender, EventArgs e)
		{
			//IL_09a2: Unknown result type (might be due to invalid IL or missing references)
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append("<Occult>");
			if (optVersion_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optVersion_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optVersion_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<OccultBeta>");
			if (optBeta_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optBeta_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optBeta_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<AAVSO>");
			if (optAAVSO_existing.get_Checked())
			{
				stringBuilder.Append(((Control)optAAVSO_existing).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optAAVSO_new).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Addresses>");
			if (optAddress_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optAddress_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optAddress_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Archive>");
			if (optArchive_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optArchive_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optArchive_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Asteroid>");
			if (optAsteroid_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optAsteroid_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optAsteroid_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Binary>");
			if (optBinary_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optBinary_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optBinary_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Classes>");
			if (optClass_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optClass_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optClass_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<CameraDelays>");
			if (optCameraDelays_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optCameraDelays_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optCameraDelays_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<deltaT>");
			if (optdeltaT_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optdeltaT_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optdeltaT_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Diameters>");
			if (optDia_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optDia_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optDia_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Gaia16>");
			if (optGaia16_Existing.get_Checked())
			{
				stringBuilder.Append(((Control)optGaia16_Existing).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optGaia14_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Gaia14>");
			if (optGaia14_Existing.get_Checked())
			{
				stringBuilder.Append(((Control)optGaia14_Existing).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optGaia14_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Gaia12>");
			if (optGaia12_Existing.get_Checked())
			{
				stringBuilder.Append(((Control)optGaia12_Existing).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optGaia12_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Gaia9>");
			if (optGaia9_Existing.get_Checked())
			{
				stringBuilder.Append(((Control)optGaia9_Existing).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optGaia9_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<ISAM>");
			if (optISAM_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optISAM_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optISAM_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<JPL_DE>");
			if (optJPL_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optJPL_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optJPL_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Kepler2>");
			if (optK2_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optK2_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optK2_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<LCurve>");
			if (optLC_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optLC_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optLC_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Lola128>");
			if (optLOLA_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optLOLA_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optLOLA_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Lunar>");
			if (optLunar_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optLunar_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optLunar_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Rings>");
			if (optRings_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optRings_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optRings_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Shapes>");
			if (optShapes_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optShapes_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optShapes_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<StarDia>");
			if (optStarDia_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optStarDia_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optStarDia_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<Tycho2>");
			if (optTycho2_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optTycho2_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optTycho2_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<TychoGaia>");
			if (optGaia12_Existing.get_Checked())
			{
				stringBuilder.Append(((Control)optGaia12_Existing).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optGaia12_New).get_Text() + "\r\n");
			}
			stringBuilder.Append("<XZ>");
			if (optXZ_Exising.get_Checked())
			{
				stringBuilder.Append(((Control)optXZ_Exising).get_Text() + "\r\n");
			}
			else
			{
				stringBuilder.Append(((Control)optXZ_New).get_Text() + "\r\n");
			}
			if (!Directory.Exists(Utilities.AppPath + "\\InstallationFiles"))
			{
				Directory.CreateDirectory(Utilities.AppPath + "\\InstallationFiles");
			}
			using (StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\InstallationFiles\\DownloadControl.txt"))
			{
				streamWriter.Write(stringBuilder.ToString());
			}
			using (StreamWriter streamWriter2 = new StreamWriter(Utilities.AppPath + "\\Resource Files\\DownloadControl.txt"))
			{
				streamWriter2.Write(stringBuilder.ToString());
			}
			string text = Utilities.AppPath + "\\InstallationFiles\\LowerCaseControl";
			if (!Directory.Exists(text))
			{
				Directory.CreateDirectory(text);
			}
			using (StreamWriter streamWriter3 = new StreamWriter(text + "\\downloadcontrol.txt"))
			{
				streamWriter3.Write(stringBuilder.ToString());
			}
			MessageBox.Show("The 'DownloadControl.txt' file has been created in the 'InstallationFiles' directory.\r\n\r\nTo update the download control file on the IOTA site, this file MUST\r\nbe uploaded before a new instance of Occult is run - as a new instance \r\nof Occult will overwrite this file with the existing version from the IOTA site", "File created");
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Download control");
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
			//IL_00bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c6: Expected O, but got Unknown
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d1: Expected O, but got Unknown
			//IL_00d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00dc: Expected O, but got Unknown
			//IL_00dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e7: Expected O, but got Unknown
			//IL_00e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f2: Expected O, but got Unknown
			//IL_00f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00fd: Expected O, but got Unknown
			//IL_00fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0108: Expected O, but got Unknown
			//IL_0109: Unknown result type (might be due to invalid IL or missing references)
			//IL_0113: Expected O, but got Unknown
			//IL_0114: Unknown result type (might be due to invalid IL or missing references)
			//IL_011e: Expected O, but got Unknown
			//IL_011f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0129: Expected O, but got Unknown
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0134: Expected O, but got Unknown
			//IL_0135: Unknown result type (might be due to invalid IL or missing references)
			//IL_013f: Expected O, but got Unknown
			//IL_0140: Unknown result type (might be due to invalid IL or missing references)
			//IL_014a: Expected O, but got Unknown
			//IL_014b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0155: Expected O, but got Unknown
			//IL_0156: Unknown result type (might be due to invalid IL or missing references)
			//IL_0160: Expected O, but got Unknown
			//IL_0161: Unknown result type (might be due to invalid IL or missing references)
			//IL_016b: Expected O, but got Unknown
			//IL_016c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0176: Expected O, but got Unknown
			//IL_0177: Unknown result type (might be due to invalid IL or missing references)
			//IL_0181: Expected O, but got Unknown
			//IL_0182: Unknown result type (might be due to invalid IL or missing references)
			//IL_018c: Expected O, but got Unknown
			//IL_018d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0197: Expected O, but got Unknown
			//IL_0198: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a2: Expected O, but got Unknown
			//IL_01a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ad: Expected O, but got Unknown
			//IL_01ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b8: Expected O, but got Unknown
			//IL_01b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c3: Expected O, but got Unknown
			//IL_01c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ce: Expected O, but got Unknown
			//IL_01cf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d9: Expected O, but got Unknown
			//IL_01da: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e4: Expected O, but got Unknown
			//IL_01e5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ef: Expected O, but got Unknown
			//IL_01f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fa: Expected O, but got Unknown
			//IL_01fb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0205: Expected O, but got Unknown
			//IL_0206: Unknown result type (might be due to invalid IL or missing references)
			//IL_0210: Expected O, but got Unknown
			//IL_0211: Unknown result type (might be due to invalid IL or missing references)
			//IL_021b: Expected O, but got Unknown
			//IL_021c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0226: Expected O, but got Unknown
			//IL_0227: Unknown result type (might be due to invalid IL or missing references)
			//IL_0231: Expected O, but got Unknown
			//IL_0232: Unknown result type (might be due to invalid IL or missing references)
			//IL_023c: Expected O, but got Unknown
			//IL_023d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0247: Expected O, but got Unknown
			//IL_0248: Unknown result type (might be due to invalid IL or missing references)
			//IL_0252: Expected O, but got Unknown
			//IL_0253: Unknown result type (might be due to invalid IL or missing references)
			//IL_025d: Expected O, but got Unknown
			//IL_025e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0268: Expected O, but got Unknown
			//IL_0269: Unknown result type (might be due to invalid IL or missing references)
			//IL_0273: Expected O, but got Unknown
			//IL_0274: Unknown result type (might be due to invalid IL or missing references)
			//IL_027e: Expected O, but got Unknown
			//IL_027f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0289: Expected O, but got Unknown
			//IL_028a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0294: Expected O, but got Unknown
			//IL_0295: Unknown result type (might be due to invalid IL or missing references)
			//IL_029f: Expected O, but got Unknown
			//IL_02a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02aa: Expected O, but got Unknown
			//IL_02ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b5: Expected O, but got Unknown
			//IL_02b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c0: Expected O, but got Unknown
			//IL_02c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cb: Expected O, but got Unknown
			//IL_02cc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d6: Expected O, but got Unknown
			//IL_02d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e1: Expected O, but got Unknown
			//IL_02e2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ec: Expected O, but got Unknown
			//IL_02ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f7: Expected O, but got Unknown
			//IL_02f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0302: Expected O, but got Unknown
			//IL_0303: Unknown result type (might be due to invalid IL or missing references)
			//IL_030d: Expected O, but got Unknown
			//IL_030e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0318: Expected O, but got Unknown
			//IL_0319: Unknown result type (might be due to invalid IL or missing references)
			//IL_0323: Expected O, but got Unknown
			//IL_0324: Unknown result type (might be due to invalid IL or missing references)
			//IL_032e: Expected O, but got Unknown
			//IL_032f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0339: Expected O, but got Unknown
			//IL_033a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0344: Expected O, but got Unknown
			//IL_0345: Unknown result type (might be due to invalid IL or missing references)
			//IL_034f: Expected O, but got Unknown
			//IL_0350: Unknown result type (might be due to invalid IL or missing references)
			//IL_035a: Expected O, but got Unknown
			//IL_035b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0365: Expected O, but got Unknown
			//IL_0366: Unknown result type (might be due to invalid IL or missing references)
			//IL_0370: Expected O, but got Unknown
			//IL_0371: Unknown result type (might be due to invalid IL or missing references)
			//IL_037b: Expected O, but got Unknown
			//IL_037c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0386: Expected O, but got Unknown
			//IL_0387: Unknown result type (might be due to invalid IL or missing references)
			//IL_0391: Expected O, but got Unknown
			//IL_0392: Unknown result type (might be due to invalid IL or missing references)
			//IL_039c: Expected O, but got Unknown
			//IL_039d: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a7: Expected O, but got Unknown
			//IL_03a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b2: Expected O, but got Unknown
			//IL_03b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03bd: Expected O, but got Unknown
			//IL_03be: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c8: Expected O, but got Unknown
			//IL_03c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d3: Expected O, but got Unknown
			//IL_03d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03de: Expected O, but got Unknown
			//IL_03df: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e9: Expected O, but got Unknown
			//IL_03ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f4: Expected O, but got Unknown
			//IL_03f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ff: Expected O, but got Unknown
			//IL_0400: Unknown result type (might be due to invalid IL or missing references)
			//IL_040a: Expected O, but got Unknown
			//IL_040b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0415: Expected O, but got Unknown
			//IL_0416: Unknown result type (might be due to invalid IL or missing references)
			//IL_0420: Expected O, but got Unknown
			//IL_0421: Unknown result type (might be due to invalid IL or missing references)
			//IL_042b: Expected O, but got Unknown
			//IL_042c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0436: Expected O, but got Unknown
			//IL_0437: Unknown result type (might be due to invalid IL or missing references)
			//IL_0441: Expected O, but got Unknown
			//IL_0442: Unknown result type (might be due to invalid IL or missing references)
			//IL_044c: Expected O, but got Unknown
			//IL_044d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0457: Expected O, but got Unknown
			//IL_0458: Unknown result type (might be due to invalid IL or missing references)
			//IL_0462: Expected O, but got Unknown
			//IL_0463: Unknown result type (might be due to invalid IL or missing references)
			//IL_046d: Expected O, but got Unknown
			//IL_046e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0478: Expected O, but got Unknown
			//IL_0479: Unknown result type (might be due to invalid IL or missing references)
			//IL_0483: Expected O, but got Unknown
			//IL_0484: Unknown result type (might be due to invalid IL or missing references)
			//IL_048e: Expected O, but got Unknown
			//IL_048f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0499: Expected O, but got Unknown
			//IL_049a: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a4: Expected O, but got Unknown
			//IL_04a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04af: Expected O, but got Unknown
			//IL_04b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ba: Expected O, but got Unknown
			//IL_04bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c5: Expected O, but got Unknown
			//IL_04c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d0: Expected O, but got Unknown
			//IL_04d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04db: Expected O, but got Unknown
			//IL_04dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e6: Expected O, but got Unknown
			//IL_04e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f1: Expected O, but got Unknown
			//IL_04f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_04fc: Expected O, but got Unknown
			//IL_04fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0507: Expected O, but got Unknown
			//IL_0508: Unknown result type (might be due to invalid IL or missing references)
			//IL_0512: Expected O, but got Unknown
			//IL_0513: Unknown result type (might be due to invalid IL or missing references)
			//IL_051d: Expected O, but got Unknown
			//IL_051e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0528: Expected O, but got Unknown
			//IL_0529: Unknown result type (might be due to invalid IL or missing references)
			//IL_0533: Expected O, but got Unknown
			//IL_0534: Unknown result type (might be due to invalid IL or missing references)
			//IL_053e: Expected O, but got Unknown
			//IL_053f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0549: Expected O, but got Unknown
			//IL_054a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0554: Expected O, but got Unknown
			//IL_0555: Unknown result type (might be due to invalid IL or missing references)
			//IL_055f: Expected O, but got Unknown
			//IL_0560: Unknown result type (might be due to invalid IL or missing references)
			//IL_056a: Expected O, but got Unknown
			//IL_056b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0575: Expected O, but got Unknown
			//IL_0576: Unknown result type (might be due to invalid IL or missing references)
			//IL_0580: Expected O, but got Unknown
			//IL_0581: Unknown result type (might be due to invalid IL or missing references)
			//IL_058b: Expected O, but got Unknown
			//IL_058c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0596: Expected O, but got Unknown
			//IL_0597: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a1: Expected O, but got Unknown
			//IL_05a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ac: Expected O, but got Unknown
			//IL_05ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b7: Expected O, but got Unknown
			//IL_05b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c2: Expected O, but got Unknown
			//IL_05c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05cd: Expected O, but got Unknown
			//IL_05ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d8: Expected O, but got Unknown
			//IL_05d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e3: Expected O, but got Unknown
			//IL_05e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05ee: Expected O, but got Unknown
			//IL_05ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f9: Expected O, but got Unknown
			optArchive_Exising = new RadioButton();
			optArchive_New = new RadioButton();
			label1 = new Label();
			label2 = new Label();
			panel1 = new Panel();
			picArchive = new PictureBox();
			panel2 = new Panel();
			picTycho2 = new PictureBox();
			optTycho2_New = new RadioButton();
			optTycho2_Exising = new RadioButton();
			panel3 = new Panel();
			picXZ = new PictureBox();
			optXZ_New = new RadioButton();
			optXZ_Exising = new RadioButton();
			panel4 = new Panel();
			picJPLDE = new PictureBox();
			optJPL_New = new RadioButton();
			optJPL_Exising = new RadioButton();
			panel6 = new Panel();
			optVersion_New = new RadioButton();
			optVersion_Exising = new RadioButton();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			cmdWrite = new Button();
			label10 = new Label();
			panel5 = new Panel();
			picAddresses = new PictureBox();
			optAddress_New = new RadioButton();
			optAddress_Exising = new RadioButton();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label11 = new Label();
			panel7 = new Panel();
			picAsteroid = new PictureBox();
			optAsteroid_New = new RadioButton();
			optAsteroid_Exising = new RadioButton();
			label12 = new Label();
			panel8 = new Panel();
			picRecent = new PictureBox();
			optLunar_New = new RadioButton();
			optLunar_Exising = new RadioButton();
			label13 = new Label();
			label14 = new Label();
			panel9 = new Panel();
			picdT = new PictureBox();
			optdeltaT_New = new RadioButton();
			optdeltaT_Exising = new RadioButton();
			label15 = new Label();
			panel10 = new Panel();
			picBinary = new PictureBox();
			optBinary_New = new RadioButton();
			optBinary_Exising = new RadioButton();
			label16 = new Label();
			panel11 = new Panel();
			picDia = new PictureBox();
			optDia_New = new RadioButton();
			optDia_Exising = new RadioButton();
			label19 = new Label();
			label20 = new Label();
			panel14 = new Panel();
			picStarDia = new PictureBox();
			optStarDia_New = new RadioButton();
			optStarDia_Exising = new RadioButton();
			label21 = new Label();
			panel15 = new Panel();
			optBeta_New = new RadioButton();
			optBeta_Exising = new RadioButton();
			label22 = new Label();
			panel16 = new Panel();
			picCameraDelays = new PictureBox();
			optCameraDelays_New = new RadioButton();
			optCameraDelays_Exising = new RadioButton();
			label23 = new Label();
			panel17 = new Panel();
			picRings = new PictureBox();
			optRings_New = new RadioButton();
			optRings_Exising = new RadioButton();
			label24 = new Label();
			panel18 = new Panel();
			picK2 = new PictureBox();
			optK2_New = new RadioButton();
			optK2_Exising = new RadioButton();
			label25 = new Label();
			panel19 = new Panel();
			picLOLA = new PictureBox();
			optLOLA_New = new RadioButton();
			optLOLA_Exising = new RadioButton();
			label26 = new Label();
			panel20 = new Panel();
			picLC = new PictureBox();
			optLC_New = new RadioButton();
			optLC_Exising = new RadioButton();
			label28 = new Label();
			panel21 = new Panel();
			picISAM = new PictureBox();
			optISAM_New = new RadioButton();
			optISAM_Exising = new RadioButton();
			label29 = new Label();
			panel22 = new Panel();
			picGaia14 = new PictureBox();
			optGaia14_New = new RadioButton();
			optGaia14_Existing = new RadioButton();
			label30 = new Label();
			panel23 = new Panel();
			picGaia12 = new PictureBox();
			optGaia12_New = new RadioButton();
			optGaia12_Existing = new RadioButton();
			label31 = new Label();
			panel24 = new Panel();
			picClass = new PictureBox();
			optClass_New = new RadioButton();
			optClass_Exising = new RadioButton();
			label32 = new Label();
			panel25 = new Panel();
			picShapes = new PictureBox();
			optShapes_New = new RadioButton();
			optShapes_Exising = new RadioButton();
			label27 = new Label();
			panel26 = new Panel();
			picGaia16 = new PictureBox();
			optGaia16_New = new RadioButton();
			optGaia16_Existing = new RadioButton();
			label17 = new Label();
			label18 = new Label();
			panel12 = new Panel();
			picGaia9 = new PictureBox();
			optGaia9_New = new RadioButton();
			optGaia9_Existing = new RadioButton();
			label33 = new Label();
			panel13 = new Panel();
			picAAVSO = new PictureBox();
			optAAVSO_new = new RadioButton();
			optAAVSO_existing = new RadioButton();
			((Control)panel1).SuspendLayout();
			((ISupportInitialize)picArchive).BeginInit();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)picTycho2).BeginInit();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)picXZ).BeginInit();
			((Control)panel4).SuspendLayout();
			((ISupportInitialize)picJPLDE).BeginInit();
			((Control)panel6).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((ISupportInitialize)picAddresses).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((ISupportInitialize)picAsteroid).BeginInit();
			((Control)panel8).SuspendLayout();
			((ISupportInitialize)picRecent).BeginInit();
			((Control)panel9).SuspendLayout();
			((ISupportInitialize)picdT).BeginInit();
			((Control)panel10).SuspendLayout();
			((ISupportInitialize)picBinary).BeginInit();
			((Control)panel11).SuspendLayout();
			((ISupportInitialize)picDia).BeginInit();
			((Control)panel14).SuspendLayout();
			((ISupportInitialize)picStarDia).BeginInit();
			((Control)panel15).SuspendLayout();
			((Control)panel16).SuspendLayout();
			((ISupportInitialize)picCameraDelays).BeginInit();
			((Control)panel17).SuspendLayout();
			((ISupportInitialize)picRings).BeginInit();
			((Control)panel18).SuspendLayout();
			((ISupportInitialize)picK2).BeginInit();
			((Control)panel19).SuspendLayout();
			((ISupportInitialize)picLOLA).BeginInit();
			((Control)panel20).SuspendLayout();
			((ISupportInitialize)picLC).BeginInit();
			((Control)panel21).SuspendLayout();
			((ISupportInitialize)picISAM).BeginInit();
			((Control)panel22).SuspendLayout();
			((ISupportInitialize)picGaia14).BeginInit();
			((Control)panel23).SuspendLayout();
			((ISupportInitialize)picGaia12).BeginInit();
			((Control)panel24).SuspendLayout();
			((ISupportInitialize)picClass).BeginInit();
			((Control)panel25).SuspendLayout();
			((ISupportInitialize)picShapes).BeginInit();
			((Control)panel26).SuspendLayout();
			((ISupportInitialize)picGaia16).BeginInit();
			((Control)panel12).SuspendLayout();
			((ISupportInitialize)picGaia9).BeginInit();
			((Control)panel13).SuspendLayout();
			((ISupportInitialize)picAAVSO).BeginInit();
			((Control)this).SuspendLayout();
			((Control)optArchive_Exising).set_AutoSize(true);
			optArchive_Exising.set_Checked(true);
			((Control)optArchive_Exising).set_Location(new Point(13, 3));
			((Control)optArchive_Exising).set_Name("optArchive_Exising");
			((Control)optArchive_Exising).set_Size(new Size(73, 17));
			((Control)optArchive_Exising).set_TabIndex(0);
			optArchive_Exising.set_TabStop(true);
			((Control)optArchive_Exising).set_Text("01011900");
			((ButtonBase)optArchive_Exising).set_UseVisualStyleBackColor(true);
			((Control)optArchive_New).set_AutoSize(true);
			((Control)optArchive_New).set_Location(new Point(131, 5));
			((Control)optArchive_New).set_Name("optArchive_New");
			((Control)optArchive_New).set_Size(new Size(14, 13));
			((Control)optArchive_New).set_TabIndex(1);
			((ButtonBase)optArchive_New).set_UseVisualStyleBackColor(true);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(117, 136));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(80, 26));
			((Control)label1).set_TabIndex(5);
			((Control)label1).set_Text("Use existing \r\nUTC dates");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(235, 136));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(67, 26));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Use new\r\nUTC dates");
			((Control)panel1).get_Controls().Add((Control)(object)picArchive);
			((Control)panel1).get_Controls().Add((Control)(object)optArchive_New);
			((Control)panel1).get_Controls().Add((Control)(object)optArchive_Exising);
			((Control)panel1).set_Location(new Point(106, 503));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(280, 22));
			((Control)panel1).set_TabIndex(21);
			picArchive.set_Image((Image)Occult.Properties.Resources.error);
			picArchive.set_InitialImage((Image)null);
			((Control)picArchive).set_Location(new Point(253, 3));
			((Control)picArchive).set_Name("picArchive");
			((Control)picArchive).set_Size(new Size(15, 16));
			picArchive.set_TabIndex(6);
			picArchive.set_TabStop(false);
			((Control)panel2).get_Controls().Add((Control)(object)picTycho2);
			((Control)panel2).get_Controls().Add((Control)(object)optTycho2_New);
			((Control)panel2).get_Controls().Add((Control)(object)optTycho2_Exising);
			((Control)panel2).set_Location(new Point(106, 660));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(280, 22));
			((Control)panel2).set_TabIndex(25);
			picTycho2.set_Image((Image)Occult.Properties.Resources.error);
			picTycho2.set_InitialImage((Image)null);
			((Control)picTycho2).set_Location(new Point(253, 3));
			((Control)picTycho2).set_Name("picTycho2");
			((Control)picTycho2).set_Size(new Size(15, 16));
			picTycho2.set_TabIndex(6);
			picTycho2.set_TabStop(false);
			((Control)optTycho2_New).set_AutoSize(true);
			((Control)optTycho2_New).set_Location(new Point(131, 5));
			((Control)optTycho2_New).set_Name("optTycho2_New");
			((Control)optTycho2_New).set_Size(new Size(14, 13));
			((Control)optTycho2_New).set_TabIndex(1);
			((ButtonBase)optTycho2_New).set_UseVisualStyleBackColor(true);
			((Control)optTycho2_Exising).set_AutoSize(true);
			optTycho2_Exising.set_Checked(true);
			((Control)optTycho2_Exising).set_Location(new Point(13, 3));
			((Control)optTycho2_Exising).set_Name("optTycho2_Exising");
			((Control)optTycho2_Exising).set_Size(new Size(73, 17));
			((Control)optTycho2_Exising).set_TabIndex(0);
			optTycho2_Exising.set_TabStop(true);
			((Control)optTycho2_Exising).set_Text("01011900");
			((ButtonBase)optTycho2_Exising).set_UseVisualStyleBackColor(true);
			((Control)panel3).get_Controls().Add((Control)(object)picXZ);
			((Control)panel3).get_Controls().Add((Control)(object)optXZ_New);
			((Control)panel3).get_Controls().Add((Control)(object)optXZ_Exising);
			((Control)panel3).set_Location(new Point(106, 304));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(280, 22));
			((Control)panel3).set_TabIndex(23);
			picXZ.set_Image((Image)Occult.Properties.Resources.error);
			picXZ.set_InitialImage((Image)null);
			((Control)picXZ).set_Location(new Point(253, 3));
			((Control)picXZ).set_Name("picXZ");
			((Control)picXZ).set_Size(new Size(15, 16));
			picXZ.set_TabIndex(6);
			picXZ.set_TabStop(false);
			((Control)optXZ_New).set_AutoSize(true);
			((Control)optXZ_New).set_Location(new Point(131, 5));
			((Control)optXZ_New).set_Name("optXZ_New");
			((Control)optXZ_New).set_Size(new Size(14, 13));
			((Control)optXZ_New).set_TabIndex(1);
			((ButtonBase)optXZ_New).set_UseVisualStyleBackColor(true);
			((Control)optXZ_Exising).set_AutoSize(true);
			optXZ_Exising.set_Checked(true);
			((Control)optXZ_Exising).set_Location(new Point(13, 3));
			((Control)optXZ_Exising).set_Name("optXZ_Exising");
			((Control)optXZ_Exising).set_Size(new Size(73, 17));
			((Control)optXZ_Exising).set_TabIndex(0);
			optXZ_Exising.set_TabStop(true);
			((Control)optXZ_Exising).set_Text("01011900");
			((ButtonBase)optXZ_Exising).set_UseVisualStyleBackColor(true);
			((Control)panel4).get_Controls().Add((Control)(object)picJPLDE);
			((Control)panel4).get_Controls().Add((Control)(object)optJPL_New);
			((Control)panel4).get_Controls().Add((Control)(object)optJPL_Exising);
			((Control)panel4).set_Location(new Point(106, 683));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(280, 22));
			((Control)panel4).set_TabIndex(27);
			picJPLDE.set_Image((Image)Occult.Properties.Resources.error);
			picJPLDE.set_InitialImage((Image)null);
			((Control)picJPLDE).set_Location(new Point(253, 3));
			((Control)picJPLDE).set_Name("picJPLDE");
			((Control)picJPLDE).set_Size(new Size(15, 16));
			picJPLDE.set_TabIndex(6);
			picJPLDE.set_TabStop(false);
			((Control)optJPL_New).set_AutoSize(true);
			((Control)optJPL_New).set_Location(new Point(131, 5));
			((Control)optJPL_New).set_Name("optJPL_New");
			((Control)optJPL_New).set_Size(new Size(14, 13));
			((Control)optJPL_New).set_TabIndex(1);
			((ButtonBase)optJPL_New).set_UseVisualStyleBackColor(true);
			((Control)optJPL_Exising).set_AutoSize(true);
			optJPL_Exising.set_Checked(true);
			((Control)optJPL_Exising).set_Location(new Point(13, 3));
			((Control)optJPL_Exising).set_Name("optJPL_Exising");
			((Control)optJPL_Exising).set_Size(new Size(73, 17));
			((Control)optJPL_Exising).set_TabIndex(0);
			optJPL_Exising.set_TabStop(true);
			((Control)optJPL_Exising).set_Text("01011900");
			((ButtonBase)optJPL_Exising).set_UseVisualStyleBackColor(true);
			((Control)panel6).get_Controls().Add((Control)(object)optVersion_New);
			((Control)panel6).get_Controls().Add((Control)(object)optVersion_Exising);
			((Control)panel6).set_Location(new Point(106, 62));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(238, 32));
			((Control)panel6).set_TabIndex(4);
			((Control)optVersion_New).set_AutoSize(true);
			((Control)optVersion_New).set_Location(new Point(131, 8));
			((Control)optVersion_New).set_Name("optVersion_New");
			((Control)optVersion_New).set_Size(new Size(14, 13));
			((Control)optVersion_New).set_TabIndex(1);
			((ButtonBase)optVersion_New).set_UseVisualStyleBackColor(true);
			((Control)optVersion_Exising).set_AutoSize(true);
			optVersion_Exising.set_Checked(true);
			((Control)optVersion_Exising).set_Location(new Point(13, 8));
			((Control)optVersion_Exising).set_Name("optVersion_Exising");
			((Control)optVersion_Exising).set_Size(new Size(58, 17));
			((Control)optVersion_Exising).set_TabIndex(0);
			optVersion_Exising.set_TabStop(true);
			((Control)optVersion_Exising).set_Text("4.0.0.0");
			((ButtonBase)optVersion_Exising).set_UseVisualStyleBackColor(true);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(235, 35));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(77, 26));
			((Control)label3).set_TabIndex(2);
			((Control)label3).set_Text("Use\r\nNew version");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(117, 35));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(96, 26));
			((Control)label4).set_TabIndex(1);
			((Control)label4).set_Text("Use\r\nExisting version");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(31, 502));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(73, 13));
			((Control)label5).set_TabIndex(20);
			((Control)label5).set_Text("Lunar Archive");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(62, 309));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(42, 13));
			((Control)label6).set_TabIndex(22);
			((Control)label6).set_Text("XZ files");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(61, 665));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(43, 13));
			((Control)label7).set_TabIndex(24);
			((Control)label7).set_Text("Tycho2");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(26, 688));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(78, 13));
			((Control)label8).set_TabIndex(26);
			((Control)label8).set_Text("JPL DE ephem");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(24, 70));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(79, 13));
			((Control)label9).set_TabIndex(3);
			((Control)label9).set_Text("Occult program");
			label9.set_TextAlign(ContentAlignment.TopCenter);
			((Control)cmdWrite).set_Location(new Point(106, 789));
			((Control)cmdWrite).set_Name("cmdWrite");
			((Control)cmdWrite).set_Size(new Size(196, 30));
			((Control)cmdWrite).set_TabIndex(34);
			((Control)cmdWrite).set_Text("Create new DownloadControl file");
			((ButtonBase)cmdWrite).set_UseVisualStyleBackColor(true);
			((Control)cmdWrite).add_Click((EventHandler)cmdWrite_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(12, 443));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(92, 13));
			((Control)label10).set_TabIndex(12);
			((Control)label10).set_Text("Addresses (Lunar)");
			((Control)panel5).get_Controls().Add((Control)(object)picAddresses);
			((Control)panel5).get_Controls().Add((Control)(object)optAddress_New);
			((Control)panel5).get_Controls().Add((Control)(object)optAddress_Exising);
			((Control)panel5).set_Location(new Point(106, 438));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(280, 22));
			((Control)panel5).set_TabIndex(13);
			picAddresses.set_Image((Image)Occult.Properties.Resources.error);
			picAddresses.set_InitialImage((Image)null);
			((Control)picAddresses).set_Location(new Point(253, 3));
			((Control)picAddresses).set_Name("picAddresses");
			((Control)picAddresses).set_Size(new Size(15, 16));
			picAddresses.set_TabIndex(6);
			picAddresses.set_TabStop(false);
			((Control)optAddress_New).set_AutoSize(true);
			((Control)optAddress_New).set_Location(new Point(131, 5));
			((Control)optAddress_New).set_Name("optAddress_New");
			((Control)optAddress_New).set_Size(new Size(14, 13));
			((Control)optAddress_New).set_TabIndex(1);
			((ButtonBase)optAddress_New).set_UseVisualStyleBackColor(true);
			((Control)optAddress_Exising).set_AutoSize(true);
			optAddress_Exising.set_Checked(true);
			((Control)optAddress_Exising).set_Location(new Point(13, 3));
			((Control)optAddress_Exising).set_Name("optAddress_Exising");
			((Control)optAddress_Exising).set_Size(new Size(73, 17));
			((Control)optAddress_Exising).set_TabIndex(0);
			optAddress_Exising.set_TabStop(true);
			((Control)optAddress_Exising).set_Text("01011900");
			((ButtonBase)optAddress_Exising).set_UseVisualStyleBackColor(true);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(405, 24));
			((Control)menuStrip1).set_TabIndex(35);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Occult.Properties.Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(36, 170));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(68, 13));
			((Control)label11).set_TabIndex(8);
			((Control)label11).set_Text("Asteroid obs.");
			((Control)panel7).get_Controls().Add((Control)(object)picAsteroid);
			((Control)panel7).get_Controls().Add((Control)(object)optAsteroid_New);
			((Control)panel7).get_Controls().Add((Control)(object)optAsteroid_Exising);
			((Control)panel7).set_Location(new Point(106, 165));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(280, 22));
			((Control)panel7).set_TabIndex(9);
			picAsteroid.set_Image((Image)Occult.Properties.Resources.error);
			picAsteroid.set_InitialImage((Image)null);
			((Control)picAsteroid).set_Location(new Point(253, 3));
			((Control)picAsteroid).set_Name("picAsteroid");
			((Control)picAsteroid).set_Size(new Size(15, 16));
			picAsteroid.set_TabIndex(5);
			picAsteroid.set_TabStop(false);
			((Control)optAsteroid_New).set_AutoSize(true);
			((Control)optAsteroid_New).set_Location(new Point(131, 5));
			((Control)optAsteroid_New).set_Name("optAsteroid_New");
			((Control)optAsteroid_New).set_Size(new Size(14, 13));
			((Control)optAsteroid_New).set_TabIndex(1);
			((ButtonBase)optAsteroid_New).set_UseVisualStyleBackColor(true);
			((Control)optAsteroid_Exising).set_AutoSize(true);
			optAsteroid_Exising.set_Checked(true);
			((Control)optAsteroid_Exising).set_Location(new Point(13, 3));
			((Control)optAsteroid_Exising).set_Name("optAsteroid_Exising");
			((Control)optAsteroid_Exising).set_Size(new Size(73, 17));
			((Control)optAsteroid_Exising).set_TabIndex(0);
			optAsteroid_Exising.set_TabStop(true);
			((Control)optAsteroid_Exising).set_Text("01011900");
			((ButtonBase)optAsteroid_Exising).set_UseVisualStyleBackColor(true);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(13, 193));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(91, 13));
			((Control)label12).set_TabIndex(10);
			((Control)label12).set_Text("Recent lunar obs.");
			((Control)panel8).get_Controls().Add((Control)(object)picRecent);
			((Control)panel8).get_Controls().Add((Control)(object)optLunar_New);
			((Control)panel8).get_Controls().Add((Control)(object)optLunar_Exising);
			((Control)panel8).set_Location(new Point(106, 188));
			((Control)panel8).set_Name("panel8");
			((Control)panel8).set_Size(new Size(280, 22));
			((Control)panel8).set_TabIndex(11);
			picRecent.set_Image((Image)Occult.Properties.Resources.error);
			picRecent.set_InitialImage((Image)null);
			((Control)picRecent).set_Location(new Point(253, 3));
			((Control)picRecent).set_Name("picRecent");
			((Control)picRecent).set_Size(new Size(15, 16));
			picRecent.set_TabIndex(6);
			picRecent.set_TabStop(false);
			((Control)optLunar_New).set_AutoSize(true);
			((Control)optLunar_New).set_Location(new Point(131, 5));
			((Control)optLunar_New).set_Name("optLunar_New");
			((Control)optLunar_New).set_Size(new Size(14, 13));
			((Control)optLunar_New).set_TabIndex(1);
			((ButtonBase)optLunar_New).set_UseVisualStyleBackColor(true);
			((Control)optLunar_Exising).set_AutoSize(true);
			optLunar_Exising.set_Checked(true);
			((Control)optLunar_Exising).set_Location(new Point(13, 3));
			((Control)optLunar_Exising).set_Name("optLunar_Exising");
			((Control)optLunar_Exising).set_Size(new Size(73, 17));
			((Control)optLunar_Exising).set_TabIndex(0);
			optLunar_Exising.set_TabStop(true);
			((Control)optLunar_Exising).set_Text("01011900");
			((ButtonBase)optLunar_Exising).set_UseVisualStyleBackColor(true);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(38, 35));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(63, 26));
			((Control)label13).set_TabIndex(0);
			((Control)label13).set_Text("Download\r\nFile");
			label13.set_TextAlign(ContentAlignment.TopCenter);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(67, 420));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(37, 13));
			((Control)label14).set_TabIndex(14);
			((Control)label14).set_Text("deltaT");
			((Control)panel9).get_Controls().Add((Control)(object)picdT);
			((Control)panel9).get_Controls().Add((Control)(object)optdeltaT_New);
			((Control)panel9).get_Controls().Add((Control)(object)optdeltaT_Exising);
			((Control)panel9).set_Location(new Point(106, 415));
			((Control)panel9).set_Name("panel9");
			((Control)panel9).set_Size(new Size(280, 22));
			((Control)panel9).set_TabIndex(15);
			picdT.set_Image((Image)Occult.Properties.Resources.error);
			picdT.set_InitialImage((Image)null);
			((Control)picdT).set_Location(new Point(253, 3));
			((Control)picdT).set_Name("picdT");
			((Control)picdT).set_Size(new Size(15, 16));
			picdT.set_TabIndex(6);
			picdT.set_TabStop(false);
			((Control)optdeltaT_New).set_AutoSize(true);
			((Control)optdeltaT_New).set_Location(new Point(131, 5));
			((Control)optdeltaT_New).set_Name("optdeltaT_New");
			((Control)optdeltaT_New).set_Size(new Size(14, 13));
			((Control)optdeltaT_New).set_TabIndex(1);
			((ButtonBase)optdeltaT_New).set_UseVisualStyleBackColor(true);
			((Control)optdeltaT_Exising).set_AutoSize(true);
			optdeltaT_Exising.set_Checked(true);
			((Control)optdeltaT_Exising).set_Location(new Point(13, 3));
			((Control)optdeltaT_Exising).set_Name("optdeltaT_Exising");
			((Control)optdeltaT_Exising).set_Size(new Size(73, 17));
			((Control)optdeltaT_Exising).set_TabIndex(0);
			optdeltaT_Exising.set_TabStop(true);
			((Control)optdeltaT_Exising).set_Text("01011900");
			((ButtonBase)optdeltaT_Exising).set_UseVisualStyleBackColor(true);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(23, 262));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(81, 13));
			((Control)label15).set_TabIndex(16);
			((Control)label15).set_Text("Binary asteroids");
			((Control)panel10).get_Controls().Add((Control)(object)picBinary);
			((Control)panel10).get_Controls().Add((Control)(object)optBinary_New);
			((Control)panel10).get_Controls().Add((Control)(object)optBinary_Exising);
			((Control)panel10).set_Location(new Point(106, 257));
			((Control)panel10).set_Name("panel10");
			((Control)panel10).set_Size(new Size(280, 22));
			((Control)panel10).set_TabIndex(17);
			picBinary.set_Image((Image)Occult.Properties.Resources.error);
			picBinary.set_InitialImage((Image)null);
			((Control)picBinary).set_Location(new Point(253, 3));
			((Control)picBinary).set_Name("picBinary");
			((Control)picBinary).set_Size(new Size(15, 16));
			picBinary.set_TabIndex(6);
			picBinary.set_TabStop(false);
			((Control)optBinary_New).set_AutoSize(true);
			((Control)optBinary_New).set_Location(new Point(131, 5));
			((Control)optBinary_New).set_Name("optBinary_New");
			((Control)optBinary_New).set_Size(new Size(14, 13));
			((Control)optBinary_New).set_TabIndex(1);
			((ButtonBase)optBinary_New).set_UseVisualStyleBackColor(true);
			((Control)optBinary_Exising).set_AutoSize(true);
			optBinary_Exising.set_Checked(true);
			((Control)optBinary_Exising).set_Location(new Point(13, 3));
			((Control)optBinary_Exising).set_Name("optBinary_Exising");
			((Control)optBinary_Exising).set_Size(new Size(73, 17));
			((Control)optBinary_Exising).set_TabIndex(0);
			optBinary_Exising.set_TabStop(true);
			((Control)optBinary_Exising).set_Text("01011900");
			((ButtonBase)optBinary_Exising).set_UseVisualStyleBackColor(true);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(11, 711));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(93, 13));
			((Control)label16).set_TabIndex(18);
			((Control)label16).set_Text("Asteroid diameters");
			((Control)panel11).get_Controls().Add((Control)(object)picDia);
			((Control)panel11).get_Controls().Add((Control)(object)optDia_New);
			((Control)panel11).get_Controls().Add((Control)(object)optDia_Exising);
			((Control)panel11).set_Location(new Point(106, 706));
			((Control)panel11).set_Name("panel11");
			((Control)panel11).set_Size(new Size(280, 22));
			((Control)panel11).set_TabIndex(19);
			picDia.set_Image((Image)Occult.Properties.Resources.error);
			picDia.set_InitialImage((Image)null);
			((Control)picDia).set_Location(new Point(253, 3));
			((Control)picDia).set_Name("picDia");
			((Control)picDia).set_Size(new Size(15, 16));
			picDia.set_TabIndex(6);
			picDia.set_TabStop(false);
			((Control)optDia_New).set_AutoSize(true);
			((Control)optDia_New).set_Location(new Point(131, 5));
			((Control)optDia_New).set_Name("optDia_New");
			((Control)optDia_New).set_Size(new Size(14, 13));
			((Control)optDia_New).set_TabIndex(1);
			((ButtonBase)optDia_New).set_UseVisualStyleBackColor(true);
			((Control)optDia_Exising).set_AutoSize(true);
			optDia_Exising.set_Checked(true);
			((Control)optDia_Exising).set_Location(new Point(13, 3));
			((Control)optDia_Exising).set_Name("optDia_Exising");
			((Control)optDia_Exising).set_Size(new Size(73, 17));
			((Control)optDia_Exising).set_TabIndex(0);
			optDia_Exising.set_TabStop(true);
			((Control)optDia_Exising).set_Text("01011900");
			((ButtonBase)optDia_Exising).set_UseVisualStyleBackColor(true);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(337, 136));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(66, 26));
			((Control)label19).set_TabIndex(7);
			((Control)label19).set_Text("No Zip file\r\nto Upload\r\n");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(30, 734));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(74, 13));
			((Control)label20).set_TabIndex(32);
			((Control)label20).set_Text("Star diameters");
			((Control)panel14).get_Controls().Add((Control)(object)picStarDia);
			((Control)panel14).get_Controls().Add((Control)(object)optStarDia_New);
			((Control)panel14).get_Controls().Add((Control)(object)optStarDia_Exising);
			((Control)panel14).set_Location(new Point(106, 729));
			((Control)panel14).set_Name("panel14");
			((Control)panel14).set_Size(new Size(280, 22));
			((Control)panel14).set_TabIndex(33);
			picStarDia.set_Image((Image)Occult.Properties.Resources.error);
			picStarDia.set_InitialImage((Image)null);
			((Control)picStarDia).set_Location(new Point(253, 3));
			((Control)picStarDia).set_Name("picStarDia");
			((Control)picStarDia).set_Size(new Size(15, 16));
			picStarDia.set_TabIndex(6);
			picStarDia.set_TabStop(false);
			((Control)optStarDia_New).set_AutoSize(true);
			((Control)optStarDia_New).set_Location(new Point(131, 5));
			((Control)optStarDia_New).set_Name("optStarDia_New");
			((Control)optStarDia_New).set_Size(new Size(14, 13));
			((Control)optStarDia_New).set_TabIndex(1);
			((ButtonBase)optStarDia_New).set_UseVisualStyleBackColor(true);
			((Control)optStarDia_Exising).set_AutoSize(true);
			optStarDia_Exising.set_Checked(true);
			((Control)optStarDia_Exising).set_Location(new Point(13, 3));
			((Control)optStarDia_Exising).set_Name("optStarDia_Exising");
			((Control)optStarDia_Exising).set_Size(new Size(73, 17));
			((Control)optStarDia_Exising).set_TabIndex(0);
			optStarDia_Exising.set_TabStop(true);
			((Control)optStarDia_Exising).set_Text("01011900");
			((ButtonBase)optStarDia_Exising).set_UseVisualStyleBackColor(true);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(39, 95));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(65, 13));
			((Control)label21).set_TabIndex(36);
			((Control)label21).set_Text("beta version");
			label21.set_TextAlign(ContentAlignment.TopCenter);
			((Control)panel15).get_Controls().Add((Control)(object)optBeta_New);
			((Control)panel15).get_Controls().Add((Control)(object)optBeta_Exising);
			((Control)panel15).set_Location(new Point(106, 86));
			((Control)panel15).set_Name("panel15");
			((Control)panel15).set_Size(new Size(238, 32));
			((Control)panel15).set_TabIndex(37);
			((Control)optBeta_New).set_AutoSize(true);
			((Control)optBeta_New).set_Location(new Point(131, 8));
			((Control)optBeta_New).set_Name("optBeta_New");
			((Control)optBeta_New).set_Size(new Size(14, 13));
			((Control)optBeta_New).set_TabIndex(1);
			((ButtonBase)optBeta_New).set_UseVisualStyleBackColor(true);
			((Control)optBeta_Exising).set_AutoSize(true);
			optBeta_Exising.set_Checked(true);
			((Control)optBeta_Exising).set_Location(new Point(13, 8));
			((Control)optBeta_Exising).set_Name("optBeta_Exising");
			((Control)optBeta_Exising).set_Size(new Size(58, 17));
			((Control)optBeta_Exising).set_TabIndex(0);
			optBeta_Exising.set_TabStop(true);
			((Control)optBeta_Exising).set_Text("4.0.0.0");
			((ButtonBase)optBeta_Exising).set_UseVisualStyleBackColor(true);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(28, 466));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(76, 13));
			((Control)label22).set_TabIndex(38);
			((Control)label22).set_Text("Camera delays");
			((Control)panel16).get_Controls().Add((Control)(object)picCameraDelays);
			((Control)panel16).get_Controls().Add((Control)(object)optCameraDelays_New);
			((Control)panel16).get_Controls().Add((Control)(object)optCameraDelays_Exising);
			((Control)panel16).set_Location(new Point(106, 461));
			((Control)panel16).set_Name("panel16");
			((Control)panel16).set_Size(new Size(280, 22));
			((Control)panel16).set_TabIndex(39);
			picCameraDelays.set_Image((Image)Occult.Properties.Resources.error);
			picCameraDelays.set_InitialImage((Image)null);
			((Control)picCameraDelays).set_Location(new Point(253, 3));
			((Control)picCameraDelays).set_Name("picCameraDelays");
			((Control)picCameraDelays).set_Size(new Size(15, 16));
			picCameraDelays.set_TabIndex(6);
			picCameraDelays.set_TabStop(false);
			((Control)optCameraDelays_New).set_AutoSize(true);
			((Control)optCameraDelays_New).set_Location(new Point(131, 5));
			((Control)optCameraDelays_New).set_Name("optCameraDelays_New");
			((Control)optCameraDelays_New).set_Size(new Size(14, 13));
			((Control)optCameraDelays_New).set_TabIndex(1);
			((ButtonBase)optCameraDelays_New).set_UseVisualStyleBackColor(true);
			((Control)optCameraDelays_Exising).set_AutoSize(true);
			optCameraDelays_Exising.set_Checked(true);
			((Control)optCameraDelays_Exising).set_Location(new Point(13, 3));
			((Control)optCameraDelays_Exising).set_Name("optCameraDelays_Exising");
			((Control)optCameraDelays_Exising).set_Size(new Size(73, 17));
			((Control)optCameraDelays_Exising).set_TabIndex(0);
			optCameraDelays_Exising.set_TabStop(true);
			((Control)optCameraDelays_Exising).set_Text("01011900");
			((ButtonBase)optCameraDelays_Exising).set_UseVisualStyleBackColor(true);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(34, 397));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(70, 13));
			((Control)label23).set_TabIndex(40);
			((Control)label23).set_Text("Asteroid rings");
			((Control)panel17).get_Controls().Add((Control)(object)picRings);
			((Control)panel17).get_Controls().Add((Control)(object)optRings_New);
			((Control)panel17).get_Controls().Add((Control)(object)optRings_Exising);
			((Control)panel17).set_Location(new Point(106, 392));
			((Control)panel17).set_Name("panel17");
			((Control)panel17).set_Size(new Size(280, 22));
			((Control)panel17).set_TabIndex(41);
			picRings.set_Image((Image)Occult.Properties.Resources.error);
			picRings.set_InitialImage((Image)null);
			((Control)picRings).set_Location(new Point(253, 3));
			((Control)picRings).set_Name("picRings");
			((Control)picRings).set_Size(new Size(15, 16));
			picRings.set_TabIndex(6);
			picRings.set_TabStop(false);
			((Control)optRings_New).set_AutoSize(true);
			((Control)optRings_New).set_Location(new Point(131, 5));
			((Control)optRings_New).set_Name("optRings_New");
			((Control)optRings_New).set_Size(new Size(14, 13));
			((Control)optRings_New).set_TabIndex(1);
			((ButtonBase)optRings_New).set_UseVisualStyleBackColor(true);
			((Control)optRings_Exising).set_AutoSize(true);
			optRings_Exising.set_Checked(true);
			((Control)optRings_Exising).set_Location(new Point(13, 3));
			((Control)optRings_Exising).set_Name("optRings_Exising");
			((Control)optRings_Exising).set_Size(new Size(73, 17));
			((Control)optRings_Exising).set_TabIndex(0);
			optRings_Exising.set_TabStop(true);
			((Control)optRings_Exising).set_Text("01011900");
			((ButtonBase)optRings_Exising).set_UseVisualStyleBackColor(true);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(58, 239));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(46, 13));
			((Control)label24).set_TabIndex(42);
			((Control)label24).set_Text("Kepler2 ");
			((Control)panel18).get_Controls().Add((Control)(object)picK2);
			((Control)panel18).get_Controls().Add((Control)(object)optK2_New);
			((Control)panel18).get_Controls().Add((Control)(object)optK2_Exising);
			((Control)panel18).set_Location(new Point(106, 234));
			((Control)panel18).set_Name("panel18");
			((Control)panel18).set_Size(new Size(280, 22));
			((Control)panel18).set_TabIndex(43);
			picK2.set_Image((Image)Occult.Properties.Resources.error);
			picK2.set_InitialImage((Image)null);
			((Control)picK2).set_Location(new Point(253, 3));
			((Control)picK2).set_Name("picK2");
			((Control)picK2).set_Size(new Size(15, 16));
			picK2.set_TabIndex(6);
			picK2.set_TabStop(false);
			((Control)optK2_New).set_AutoSize(true);
			((Control)optK2_New).set_Location(new Point(131, 5));
			((Control)optK2_New).set_Name("optK2_New");
			((Control)optK2_New).set_Size(new Size(14, 13));
			((Control)optK2_New).set_TabIndex(1);
			((ButtonBase)optK2_New).set_UseVisualStyleBackColor(true);
			((Control)optK2_Exising).set_AutoSize(true);
			optK2_Exising.set_Checked(true);
			((Control)optK2_Exising).set_Location(new Point(13, 3));
			((Control)optK2_Exising).set_Name("optK2_Exising");
			((Control)optK2_Exising).set_Size(new Size(73, 17));
			((Control)optK2_Exising).set_TabIndex(0);
			optK2_Exising.set_TabStop(true);
			((Control)optK2_Exising).set_Text("01011900");
			((ButtonBase)optK2_Exising).set_UseVisualStyleBackColor(true);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(45, 531));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(59, 13));
			((Control)label25).set_TabIndex(44);
			((Control)label25).set_Text("LRO LOLA");
			((Control)panel19).get_Controls().Add((Control)(object)picLOLA);
			((Control)panel19).get_Controls().Add((Control)(object)optLOLA_New);
			((Control)panel19).get_Controls().Add((Control)(object)optLOLA_Exising);
			((Control)panel19).set_Location(new Point(106, 526));
			((Control)panel19).set_Name("panel19");
			((Control)panel19).set_Size(new Size(280, 22));
			((Control)panel19).set_TabIndex(45);
			picLOLA.set_Image((Image)Occult.Properties.Resources.error);
			picLOLA.set_InitialImage((Image)null);
			((Control)picLOLA).set_Location(new Point(253, 3));
			((Control)picLOLA).set_Name("picLOLA");
			((Control)picLOLA).set_Size(new Size(15, 16));
			picLOLA.set_TabIndex(6);
			picLOLA.set_TabStop(false);
			((Control)optLOLA_New).set_AutoSize(true);
			((Control)optLOLA_New).set_Location(new Point(131, 5));
			((Control)optLOLA_New).set_Name("optLOLA_New");
			((Control)optLOLA_New).set_Size(new Size(14, 13));
			((Control)optLOLA_New).set_TabIndex(1);
			((ButtonBase)optLOLA_New).set_UseVisualStyleBackColor(true);
			((Control)optLOLA_Exising).set_AutoSize(true);
			optLOLA_Exising.set_Checked(true);
			((Control)optLOLA_Exising).set_Location(new Point(13, 3));
			((Control)optLOLA_Exising).set_Name("optLOLA_Exising");
			((Control)optLOLA_Exising).set_Size(new Size(73, 17));
			((Control)optLOLA_Exising).set_TabIndex(0);
			optLOLA_Exising.set_TabStop(true);
			((Control)optLOLA_Exising).set_Text("01011900");
			((ButtonBase)optLOLA_Exising).set_UseVisualStyleBackColor(true);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Location(new Point(39, 216));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(65, 13));
			((Control)label26).set_TabIndex(46);
			((Control)label26).set_Text("Light curves");
			((Control)panel20).get_Controls().Add((Control)(object)picLC);
			((Control)panel20).get_Controls().Add((Control)(object)optLC_New);
			((Control)panel20).get_Controls().Add((Control)(object)optLC_Exising);
			((Control)panel20).set_Location(new Point(106, 211));
			((Control)panel20).set_Name("panel20");
			((Control)panel20).set_Size(new Size(280, 22));
			((Control)panel20).set_TabIndex(47);
			picLC.set_Image((Image)Occult.Properties.Resources.error);
			picLC.set_InitialImage((Image)null);
			((Control)picLC).set_Location(new Point(253, 3));
			((Control)picLC).set_Name("picLC");
			((Control)picLC).set_Size(new Size(15, 16));
			picLC.set_TabIndex(6);
			picLC.set_TabStop(false);
			((Control)optLC_New).set_AutoSize(true);
			((Control)optLC_New).set_Location(new Point(131, 5));
			((Control)optLC_New).set_Name("optLC_New");
			((Control)optLC_New).set_Size(new Size(14, 13));
			((Control)optLC_New).set_TabIndex(1);
			((ButtonBase)optLC_New).set_UseVisualStyleBackColor(true);
			((Control)optLC_Exising).set_AutoSize(true);
			optLC_Exising.set_Checked(true);
			((Control)optLC_Exising).set_Location(new Point(13, 3));
			((Control)optLC_Exising).set_Name("optLC_Exising");
			((Control)optLC_Exising).set_Size(new Size(73, 17));
			((Control)optLC_Exising).set_TabIndex(0);
			optLC_Exising.set_TabStop(true);
			((Control)optLC_Exising).set_Text("01011900");
			((ButtonBase)optLC_Exising).set_UseVisualStyleBackColor(true);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(26, 332));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(78, 13));
			((Control)label28).set_TabIndex(49);
			((Control)label28).set_Text("ISAM asteroids");
			((Control)panel21).get_Controls().Add((Control)(object)picISAM);
			((Control)panel21).get_Controls().Add((Control)(object)optISAM_New);
			((Control)panel21).get_Controls().Add((Control)(object)optISAM_Exising);
			((Control)panel21).set_Location(new Point(106, 327));
			((Control)panel21).set_Name("panel21");
			((Control)panel21).set_Size(new Size(280, 22));
			((Control)panel21).set_TabIndex(50);
			picISAM.set_Image((Image)Occult.Properties.Resources.error);
			picISAM.set_InitialImage((Image)null);
			((Control)picISAM).set_Location(new Point(253, 3));
			((Control)picISAM).set_Name("picISAM");
			((Control)picISAM).set_Size(new Size(15, 16));
			picISAM.set_TabIndex(6);
			picISAM.set_TabStop(false);
			((Control)optISAM_New).set_AutoSize(true);
			((Control)optISAM_New).set_Location(new Point(131, 5));
			((Control)optISAM_New).set_Name("optISAM_New");
			((Control)optISAM_New).set_Size(new Size(14, 13));
			((Control)optISAM_New).set_TabIndex(1);
			((ButtonBase)optISAM_New).set_UseVisualStyleBackColor(true);
			((Control)optISAM_Exising).set_AutoSize(true);
			optISAM_Exising.set_Checked(true);
			((Control)optISAM_Exising).set_Location(new Point(13, 3));
			((Control)optISAM_Exising).set_Name("optISAM_Exising");
			((Control)optISAM_Exising).set_Size(new Size(73, 17));
			((Control)optISAM_Exising).set_TabIndex(0);
			optISAM_Exising.set_TabStop(true);
			((Control)optISAM_Exising).set_Text("01011900");
			((ButtonBase)optISAM_Exising).set_UseVisualStyleBackColor(true);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Location(new Point(28, 619));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(76, 13));
			((Control)label29).set_TabIndex(51);
			((Control)label29).set_Text("Gaia14_EDR3");
			((Control)panel22).get_Controls().Add((Control)(object)picGaia14);
			((Control)panel22).get_Controls().Add((Control)(object)optGaia14_New);
			((Control)panel22).get_Controls().Add((Control)(object)optGaia14_Existing);
			((Control)panel22).set_Location(new Point(106, 614));
			((Control)panel22).set_Name("panel22");
			((Control)panel22).set_Size(new Size(280, 22));
			((Control)panel22).set_TabIndex(52);
			picGaia14.set_Image((Image)Occult.Properties.Resources.error);
			picGaia14.set_InitialImage((Image)null);
			((Control)picGaia14).set_Location(new Point(253, 3));
			((Control)picGaia14).set_Name("picGaia14");
			((Control)picGaia14).set_Size(new Size(15, 16));
			picGaia14.set_TabIndex(6);
			picGaia14.set_TabStop(false);
			((Control)optGaia14_New).set_AutoSize(true);
			((Control)optGaia14_New).set_Location(new Point(131, 5));
			((Control)optGaia14_New).set_Name("optGaia14_New");
			((Control)optGaia14_New).set_Size(new Size(14, 13));
			((Control)optGaia14_New).set_TabIndex(1);
			((ButtonBase)optGaia14_New).set_UseVisualStyleBackColor(true);
			((Control)optGaia14_Existing).set_AutoSize(true);
			optGaia14_Existing.set_Checked(true);
			((Control)optGaia14_Existing).set_Location(new Point(13, 3));
			((Control)optGaia14_Existing).set_Name("optGaia14_Existing");
			((Control)optGaia14_Existing).set_Size(new Size(73, 17));
			((Control)optGaia14_Existing).set_TabIndex(0);
			optGaia14_Existing.set_TabStop(true);
			((Control)optGaia14_Existing).set_Text("01011900");
			((ButtonBase)optGaia14_Existing).set_UseVisualStyleBackColor(true);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Location(new Point(28, 596));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(76, 13));
			((Control)label30).set_TabIndex(53);
			((Control)label30).set_Text("Gaia12_EDR3");
			((Control)panel23).get_Controls().Add((Control)(object)picGaia12);
			((Control)panel23).get_Controls().Add((Control)(object)optGaia12_New);
			((Control)panel23).get_Controls().Add((Control)(object)optGaia12_Existing);
			((Control)panel23).set_Location(new Point(106, 591));
			((Control)panel23).set_Name("panel23");
			((Control)panel23).set_Size(new Size(280, 22));
			((Control)panel23).set_TabIndex(54);
			picGaia12.set_Image((Image)Occult.Properties.Resources.error);
			picGaia12.set_InitialImage((Image)null);
			((Control)picGaia12).set_Location(new Point(253, 3));
			((Control)picGaia12).set_Name("picGaia12");
			((Control)picGaia12).set_Size(new Size(15, 16));
			picGaia12.set_TabIndex(6);
			picGaia12.set_TabStop(false);
			((Control)optGaia12_New).set_AutoSize(true);
			((Control)optGaia12_New).set_Location(new Point(131, 5));
			((Control)optGaia12_New).set_Name("optGaia12_New");
			((Control)optGaia12_New).set_Size(new Size(14, 13));
			((Control)optGaia12_New).set_TabIndex(1);
			((ButtonBase)optGaia12_New).set_UseVisualStyleBackColor(true);
			((Control)optGaia12_Existing).set_AutoSize(true);
			optGaia12_Existing.set_Checked(true);
			((Control)optGaia12_Existing).set_Location(new Point(13, 3));
			((Control)optGaia12_Existing).set_Name("optGaia12_Existing");
			((Control)optGaia12_Existing).set_Size(new Size(73, 17));
			((Control)optGaia12_Existing).set_TabIndex(0);
			optGaia12_Existing.set_TabStop(true);
			((Control)optGaia12_Existing).set_Text("01011900");
			((ButtonBase)optGaia12_Existing).set_UseVisualStyleBackColor(true);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(23, 286));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(83, 13));
			((Control)label31).set_TabIndex(55);
			((Control)label31).set_Text("Asteroid classes");
			((Control)panel24).get_Controls().Add((Control)(object)picClass);
			((Control)panel24).get_Controls().Add((Control)(object)optClass_New);
			((Control)panel24).get_Controls().Add((Control)(object)optClass_Exising);
			((Control)panel24).set_Location(new Point(106, 281));
			((Control)panel24).set_Name("panel24");
			((Control)panel24).set_Size(new Size(280, 22));
			((Control)panel24).set_TabIndex(56);
			picClass.set_Image((Image)Occult.Properties.Resources.error);
			picClass.set_InitialImage((Image)null);
			((Control)picClass).set_Location(new Point(253, 3));
			((Control)picClass).set_Name("picClass");
			((Control)picClass).set_Size(new Size(15, 16));
			picClass.set_TabIndex(6);
			picClass.set_TabStop(false);
			((Control)optClass_New).set_AutoSize(true);
			((Control)optClass_New).set_Location(new Point(131, 5));
			((Control)optClass_New).set_Name("optClass_New");
			((Control)optClass_New).set_Size(new Size(14, 13));
			((Control)optClass_New).set_TabIndex(1);
			((ButtonBase)optClass_New).set_UseVisualStyleBackColor(true);
			((Control)optClass_Exising).set_AutoSize(true);
			optClass_Exising.set_Checked(true);
			((Control)optClass_Exising).set_Location(new Point(13, 3));
			((Control)optClass_Exising).set_Name("optClass_Exising");
			((Control)optClass_Exising).set_Size(new Size(73, 17));
			((Control)optClass_Exising).set_TabIndex(0);
			optClass_Exising.set_TabStop(true);
			((Control)optClass_Exising).set_Text("01011900");
			((ButtonBase)optClass_Exising).set_UseVisualStyleBackColor(true);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(30, 355));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(74, 13));
			((Control)label32).set_TabIndex(57);
			((Control)label32).set_Text("Shape models");
			((Control)panel25).get_Controls().Add((Control)(object)picShapes);
			((Control)panel25).get_Controls().Add((Control)(object)optShapes_New);
			((Control)panel25).get_Controls().Add((Control)(object)optShapes_Exising);
			((Control)panel25).set_Location(new Point(106, 350));
			((Control)panel25).set_Name("panel25");
			((Control)panel25).set_Size(new Size(280, 22));
			((Control)panel25).set_TabIndex(58);
			picShapes.set_Image((Image)Occult.Properties.Resources.error);
			picShapes.set_InitialImage((Image)null);
			((Control)picShapes).set_Location(new Point(253, 3));
			((Control)picShapes).set_Name("picShapes");
			((Control)picShapes).set_Size(new Size(15, 16));
			picShapes.set_TabIndex(6);
			picShapes.set_TabStop(false);
			((Control)optShapes_New).set_AutoSize(true);
			((Control)optShapes_New).set_Location(new Point(131, 5));
			((Control)optShapes_New).set_Name("optShapes_New");
			((Control)optShapes_New).set_Size(new Size(14, 13));
			((Control)optShapes_New).set_TabIndex(1);
			((ButtonBase)optShapes_New).set_UseVisualStyleBackColor(true);
			((Control)optShapes_Exising).set_AutoSize(true);
			optShapes_Exising.set_Checked(true);
			((Control)optShapes_Exising).set_Location(new Point(13, 3));
			((Control)optShapes_Exising).set_Name("optShapes_Exising");
			((Control)optShapes_Exising).set_Size(new Size(73, 17));
			((Control)optShapes_Exising).set_TabIndex(0);
			optShapes_Exising.set_TabStop(true);
			((Control)optShapes_Exising).set_Text("01011900");
			((ButtonBase)optShapes_Exising).set_UseVisualStyleBackColor(true);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Location(new Point(28, 642));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(76, 13));
			((Control)label27).set_TabIndex(59);
			((Control)label27).set_Text("Gaia16_EDR3");
			((Control)panel26).get_Controls().Add((Control)(object)picGaia16);
			((Control)panel26).get_Controls().Add((Control)(object)optGaia16_New);
			((Control)panel26).get_Controls().Add((Control)(object)optGaia16_Existing);
			((Control)panel26).set_Location(new Point(105, 637));
			((Control)panel26).set_Name("panel26");
			((Control)panel26).set_Size(new Size(280, 22));
			((Control)panel26).set_TabIndex(60);
			picGaia16.set_Image((Image)Occult.Properties.Resources.error);
			picGaia16.set_InitialImage((Image)null);
			((Control)picGaia16).set_Location(new Point(253, 3));
			((Control)picGaia16).set_Name("picGaia16");
			((Control)picGaia16).set_Size(new Size(15, 16));
			picGaia16.set_TabIndex(6);
			picGaia16.set_TabStop(false);
			((Control)optGaia16_New).set_AutoSize(true);
			((Control)optGaia16_New).set_Location(new Point(131, 5));
			((Control)optGaia16_New).set_Name("optGaia16_New");
			((Control)optGaia16_New).set_Size(new Size(14, 13));
			((Control)optGaia16_New).set_TabIndex(1);
			((ButtonBase)optGaia16_New).set_UseVisualStyleBackColor(true);
			((Control)optGaia16_Existing).set_AutoSize(true);
			optGaia16_Existing.set_Checked(true);
			((Control)optGaia16_Existing).set_Location(new Point(13, 3));
			((Control)optGaia16_Existing).set_Name("optGaia16_Existing");
			((Control)optGaia16_Existing).set_Size(new Size(73, 17));
			((Control)optGaia16_Existing).set_TabIndex(0);
			optGaia16_Existing.set_TabStop(true);
			((Control)optGaia16_Existing).set_Text("01011900");
			((ButtonBase)optGaia16_Existing).set_UseVisualStyleBackColor(true);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 6.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_ForeColor(Color.DarkOliveGreen);
			((Control)label17).set_Location(new Point(7, 513));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(97, 12));
			((Control)label17).set_TabIndex(61);
			((Control)label17).set_Text("RArchive...2016-2025");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(34, 573));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(70, 13));
			((Control)label18).set_TabIndex(62);
			((Control)label18).set_Text("Gaia9_EDR3");
			((Control)panel12).get_Controls().Add((Control)(object)picGaia9);
			((Control)panel12).get_Controls().Add((Control)(object)optGaia9_New);
			((Control)panel12).get_Controls().Add((Control)(object)optGaia9_Existing);
			((Control)panel12).set_Location(new Point(106, 568));
			((Control)panel12).set_Name("panel12");
			((Control)panel12).set_Size(new Size(280, 22));
			((Control)panel12).set_TabIndex(63);
			picGaia9.set_Image((Image)Occult.Properties.Resources.error);
			picGaia9.set_InitialImage((Image)null);
			((Control)picGaia9).set_Location(new Point(253, 3));
			((Control)picGaia9).set_Name("picGaia9");
			((Control)picGaia9).set_Size(new Size(15, 16));
			picGaia9.set_TabIndex(6);
			picGaia9.set_TabStop(false);
			((Control)optGaia9_New).set_AutoSize(true);
			((Control)optGaia9_New).set_Location(new Point(131, 5));
			((Control)optGaia9_New).set_Name("optGaia9_New");
			((Control)optGaia9_New).set_Size(new Size(14, 13));
			((Control)optGaia9_New).set_TabIndex(1);
			((ButtonBase)optGaia9_New).set_UseVisualStyleBackColor(true);
			((Control)optGaia9_Existing).set_AutoSize(true);
			optGaia9_Existing.set_Checked(true);
			((Control)optGaia9_Existing).set_Location(new Point(13, 3));
			((Control)optGaia9_Existing).set_Name("optGaia9_Existing");
			((Control)optGaia9_Existing).set_Size(new Size(73, 17));
			((Control)optGaia9_Existing).set_TabIndex(0);
			optGaia9_Existing.set_TabStop(true);
			((Control)optGaia9_Existing).set_Text("01011900");
			((ButtonBase)optGaia9_Existing).set_UseVisualStyleBackColor(true);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Location(new Point(32, 757));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(72, 13));
			((Control)label33).set_TabIndex(64);
			((Control)label33).set_Text("AAVSO Index");
			((Control)panel13).get_Controls().Add((Control)(object)picAAVSO);
			((Control)panel13).get_Controls().Add((Control)(object)optAAVSO_new);
			((Control)panel13).get_Controls().Add((Control)(object)optAAVSO_existing);
			((Control)panel13).set_Location(new Point(105, 752));
			((Control)panel13).set_Name("panel13");
			((Control)panel13).set_Size(new Size(280, 22));
			((Control)panel13).set_TabIndex(65);
			picAAVSO.set_Image((Image)Occult.Properties.Resources.error);
			picAAVSO.set_InitialImage((Image)null);
			((Control)picAAVSO).set_Location(new Point(253, 3));
			((Control)picAAVSO).set_Name("picAAVSO");
			((Control)picAAVSO).set_Size(new Size(15, 16));
			picAAVSO.set_TabIndex(6);
			picAAVSO.set_TabStop(false);
			((Control)optAAVSO_new).set_AutoSize(true);
			((Control)optAAVSO_new).set_Location(new Point(131, 5));
			((Control)optAAVSO_new).set_Name("optAAVSO_new");
			((Control)optAAVSO_new).set_Size(new Size(14, 13));
			((Control)optAAVSO_new).set_TabIndex(1);
			((ButtonBase)optAAVSO_new).set_UseVisualStyleBackColor(true);
			((Control)optAAVSO_existing).set_AutoSize(true);
			optAAVSO_existing.set_Checked(true);
			((Control)optAAVSO_existing).set_Location(new Point(13, 3));
			((Control)optAAVSO_existing).set_Name("optAAVSO_existing");
			((Control)optAAVSO_existing).set_Size(new Size(73, 17));
			((Control)optAAVSO_existing).set_TabIndex(0);
			optAAVSO_existing.set_TabStop(true);
			((Control)optAAVSO_existing).set_Text("01011900");
			((ButtonBase)optAAVSO_existing).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(405, 827));
			((Control)this).get_Controls().Add((Control)(object)label33);
			((Control)this).get_Controls().Add((Control)(object)panel13);
			((Control)this).get_Controls().Add((Control)(object)label18);
			((Control)this).get_Controls().Add((Control)(object)panel12);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)label27);
			((Control)this).get_Controls().Add((Control)(object)panel26);
			((Control)this).get_Controls().Add((Control)(object)label32);
			((Control)this).get_Controls().Add((Control)(object)panel25);
			((Control)this).get_Controls().Add((Control)(object)label31);
			((Control)this).get_Controls().Add((Control)(object)panel24);
			((Control)this).get_Controls().Add((Control)(object)label30);
			((Control)this).get_Controls().Add((Control)(object)panel23);
			((Control)this).get_Controls().Add((Control)(object)label29);
			((Control)this).get_Controls().Add((Control)(object)panel22);
			((Control)this).get_Controls().Add((Control)(object)label28);
			((Control)this).get_Controls().Add((Control)(object)panel21);
			((Control)this).get_Controls().Add((Control)(object)label26);
			((Control)this).get_Controls().Add((Control)(object)panel20);
			((Control)this).get_Controls().Add((Control)(object)label25);
			((Control)this).get_Controls().Add((Control)(object)panel19);
			((Control)this).get_Controls().Add((Control)(object)label24);
			((Control)this).get_Controls().Add((Control)(object)panel18);
			((Control)this).get_Controls().Add((Control)(object)label23);
			((Control)this).get_Controls().Add((Control)(object)panel17);
			((Control)this).get_Controls().Add((Control)(object)label22);
			((Control)this).get_Controls().Add((Control)(object)panel16);
			((Control)this).get_Controls().Add((Control)(object)label21);
			((Control)this).get_Controls().Add((Control)(object)panel15);
			((Control)this).get_Controls().Add((Control)(object)label20);
			((Control)this).get_Controls().Add((Control)(object)panel14);
			((Control)this).get_Controls().Add((Control)(object)label19);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_Controls().Add((Control)(object)panel9);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)panel10);
			((Control)this).get_Controls().Add((Control)(object)label16);
			((Control)this).get_Controls().Add((Control)(object)panel11);
			((Control)this).get_Controls().Add((Control)(object)label13);
			((Control)this).get_Controls().Add((Control)(object)label11);
			((Control)this).get_Controls().Add((Control)(object)panel7);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)panel8);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)panel5);
			((Control)this).get_Controls().Add((Control)(object)cmdWrite);
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)label8);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)panel6);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panel4);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("DownloadControl");
			((Control)this).set_Text("Create 'DownloadControl.txt' file");
			((Form)this).add_Load((EventHandler)DownloadControl_Load);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((ISupportInitialize)picArchive).EndInit();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)picTycho2).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)picXZ).EndInit();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((ISupportInitialize)picJPLDE).EndInit();
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((ISupportInitialize)picAddresses).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((ISupportInitialize)picAsteroid).EndInit();
			((Control)panel8).ResumeLayout(false);
			((Control)panel8).PerformLayout();
			((ISupportInitialize)picRecent).EndInit();
			((Control)panel9).ResumeLayout(false);
			((Control)panel9).PerformLayout();
			((ISupportInitialize)picdT).EndInit();
			((Control)panel10).ResumeLayout(false);
			((Control)panel10).PerformLayout();
			((ISupportInitialize)picBinary).EndInit();
			((Control)panel11).ResumeLayout(false);
			((Control)panel11).PerformLayout();
			((ISupportInitialize)picDia).EndInit();
			((Control)panel14).ResumeLayout(false);
			((Control)panel14).PerformLayout();
			((ISupportInitialize)picStarDia).EndInit();
			((Control)panel15).ResumeLayout(false);
			((Control)panel15).PerformLayout();
			((Control)panel16).ResumeLayout(false);
			((Control)panel16).PerformLayout();
			((ISupportInitialize)picCameraDelays).EndInit();
			((Control)panel17).ResumeLayout(false);
			((Control)panel17).PerformLayout();
			((ISupportInitialize)picRings).EndInit();
			((Control)panel18).ResumeLayout(false);
			((Control)panel18).PerformLayout();
			((ISupportInitialize)picK2).EndInit();
			((Control)panel19).ResumeLayout(false);
			((Control)panel19).PerformLayout();
			((ISupportInitialize)picLOLA).EndInit();
			((Control)panel20).ResumeLayout(false);
			((Control)panel20).PerformLayout();
			((ISupportInitialize)picLC).EndInit();
			((Control)panel21).ResumeLayout(false);
			((Control)panel21).PerformLayout();
			((ISupportInitialize)picISAM).EndInit();
			((Control)panel22).ResumeLayout(false);
			((Control)panel22).PerformLayout();
			((ISupportInitialize)picGaia14).EndInit();
			((Control)panel23).ResumeLayout(false);
			((Control)panel23).PerformLayout();
			((ISupportInitialize)picGaia12).EndInit();
			((Control)panel24).ResumeLayout(false);
			((Control)panel24).PerformLayout();
			((ISupportInitialize)picClass).EndInit();
			((Control)panel25).ResumeLayout(false);
			((Control)panel25).PerformLayout();
			((ISupportInitialize)picShapes).EndInit();
			((Control)panel26).ResumeLayout(false);
			((Control)panel26).PerformLayout();
			((ISupportInitialize)picGaia16).EndInit();
			((Control)panel12).ResumeLayout(false);
			((Control)panel12).PerformLayout();
			((ISupportInitialize)picGaia9).EndInit();
			((Control)panel13).ResumeLayout(false);
			((Control)panel13).PerformLayout();
			((ISupportInitialize)picAAVSO).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
