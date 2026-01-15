using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class Lunar_FindAnOccultation : Form
	{
		private readonly string AppPath;

		private readonly string ZCNameFile;

		private const double Radian = 180.0 / Math.PI;

		internal ArrayList StarList = new ArrayList();

		private ArrayList JDList = new ArrayList();

		private bool FormCreated;

		private bool ChangingStar;

		private int SearchType;

		private int StarNo;

		private int PlanetNumber;

		private int AsteroidNumber;

		private double RAStar;

		private double DecStar;

		private double RAStar2000;

		private double DecStar2000;

		internal string EventLabel;

		internal string ObjectID;

		private bool Cancel;

		private string[] OpenClusters = new string[34]
		{
			"[None]                          ", "Pleiades  3 46 59  24  7  0  3.0", "Praesepe  8 40  6  19 59  0  6.0", "M4       16 23 36 -26 32  0  7.9", "M8       18 18 48 -24 23  0  5.8", "M9       17 19 12 -18 31  0  9.9", "M19      17  2 36 -26 16  0  9.2", "M21      18 24 36 -22 30  0  8.0", "M22      18 36 24 -23 54  0 11.0", "M23      17 56 48 -19  1  0 10.0",
			"M25      18 31 36 -19 15  0  6.6", "M28      18 24 30 -24 52  0  8.9", "M35       6  8 54  24 20  0  8.0", "M67       8 50 24  11 49  0  9.0", "M72      20 53 30 -12 32  0 11.4", "M73      20 59  0 -12 38  0 11.0", "M75      20  6  6 -21 55  0 10.6", "M80      16 17  0 -22 59  0  9.2", "NGC1647   4 46  0  19  4  0  8.4", "NGC1746   5  3 18  23 49  0  8.0",
			"NGC2129   6  1  0  23 18  0  8.7", "NGC2158   6  7 30  24  6  0 10.6", "NGC5897  15 17 24 -21  1  0 10.6", "NGC6144  16 27 18 -26  2  0 11.1", "NGC6284  17  4 30 -24 46  0 11.0", "NGC6293  17 10 12 -26 35  0 10.2", "NGC6316  17 16 36 -28  8  0 11.0", "NGC6355  17 24  0 -26 21  0 11.6", "NGC6440  17 48 54 -20 22  0 11.7", "NGC6544  18  7 18 -25  0  0 10.3",
			"NGC6553  18  9 18 -25 54  0 10.3", "NGC6595  18 17  0 -19 53  0  9.0", "NGC6638  18 30 54 -25 30  0 11.2", "NGC6642  18 31 54 -23 29  0 10.8"
		};

		private IContainer components;

		private GroupBox grpStars;

		private GroupBox grpPlanets;

		private GroupBox grpAsteroids;

		private RadioButton optPluto;

		private RadioButton optNeptune;

		private RadioButton optUranus;

		private RadioButton optSaturn;

		private RadioButton optJupiter;

		private RadioButton optMars;

		private RadioButton optVenus;

		private RadioButton optMercury;

		private GroupBox grpCoordinates;

		private ComboBox cmbAsteroid;

		private TextBox txtXZ;

		private TextBox txtSAO;

		private TextBox txtZC;

		private ComboBox cmbNames;

		private RadioButton optMag4;

		private RadioButton optMag3;

		private RadioButton optMag2;

		private RadioButton optAll;

		private RadioButton optBayer;

		private RadioButton optProper;

		private GroupBox groupBox2;

		private Label label3;

		private Label label2;

		private Label label1;

		private GroupBox groupBox1;

		private Button cmdFindStar;

		private Button cmdFindPlanet;

		private Button cmdFindAsteroid;

		private Button cmdFindObject;

		private Label label13;

		private Label label4;

		private Label label5;

		private Label label6;

		private TextBox txtRS1;

		private TextBox txtRM1;

		private TextBox txtRH1;

		private Label label15;

		private Label label7;

		private TextBox txtDS1;

		private TextBox txtDM1;

		private TextBox txtDD1;

		private Label label8;

		private Label label9;

		private NumericUpDown updnStartYear;

		private NumericUpDown updnEndYear;

		private Label label10;

		private NumericUpDown updnMag;

		private Label label11;

		private TextBox txtLabel;

		private Label label12;

		private Label label14;

		private Label label16;

		private ProgressBar pbarFind;

		private Button cmdPlotAll;

		internal ListBox lstEvents;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem plotAsWorldMapToolStripMenuItem;

		private ToolStripMenuItem plotALLListedEventsToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private ToolStripMenuItem viewInGoogleEarthToolStripMenuItem;

		private ToolStripMenuItem saveGoogleEarthKMLFileToolStripMenuItem;

		private ToolStripMenuItem saveGoogleMapHTMFileToolStripMenuItem;

		private Label label17;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListOfEventsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem hToolStripMenuItem;

		private Button cmdCancel;

		private RadioButton optColour;

		private RadioButton optBW;

		private Label label18;

		private ComboBox cmbClusters;

		public Lunar_FindAnOccultation()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			ZCNameFile = AppPath + "\\Resource Files\\ZCNames.dat";
		}

		private void Lunar_FindAnOccultation_Load(object sender, EventArgs e)
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
			cmbAsteroid.get_Items().Clear();
			if (Elements.UserAsteroids_Lunar.AstElements.Count < 1)
			{
				Elements.UserAsteroids_Lunar.Fill_AllAsteroids();
			}
			for (int i = 0; i < Elements.UserAsteroids_Lunar.AstElements.Count; i++)
			{
				cmbAsteroid.get_Items().Add((object)(Elements.UserAsteroids_Lunar.AstElements[i].IDNumber.ToString().PadLeft(7) + " " + Elements.UserAsteroids_Lunar.AstElements[i].IDName));
			}
			if (cmbAsteroid.get_Items().get_Count() > 0)
			{
				((ListControl)cmbAsteroid).set_SelectedIndex(0);
			}
			updnStartYear.set_Value((decimal)DateTime.Now.Year - 1m);
			updnEndYear.set_Value((decimal)DateTime.Now.Year + 5m);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Visible(Settings.Default.GoogleEarthInstalled);
			FormCreated = true;
			optMag2.set_Checked(true);
			cmbClusters.get_Items().Clear();
			for (int j = 0; j < OpenClusters.GetUpperBound(0); j++)
			{
				cmbClusters.get_Items().Add((object)OpenClusters[j].Substring(0, 8).Trim());
			}
			((ListControl)cmbClusters).set_SelectedIndex(0);
		}

		private void optProper_CheckedChanged(object sender, EventArgs e)
		{
			CreateStarList(0);
		}

		private void optBayer_CheckedChanged(object sender, EventArgs e)
		{
			CreateStarList(1);
		}

		private void optAll_CheckedChanged(object sender, EventArgs e)
		{
			CreateStarList(2);
		}

		private void optMag2_CheckedChanged(object sender, EventArgs e)
		{
			CreateStarList(3);
		}

		private void optMag3_CheckedChanged(object sender, EventArgs e)
		{
			CreateStarList(4);
		}

		private void optMag4_CheckedChanged(object sender, EventArgs e)
		{
			CreateStarList(5);
		}

		private void CreateStarList(int ListType)
		{
			if (!FormCreated)
			{
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			StarList.Clear();
			cmbNames.get_Items().Clear();
			cmbNames.set_Sorted(ListType != 1 && ListType != 2);
			StarList.Add(0);
			cmbNames.get_Items().Add((object)"");
			StreamReader streamReader = new StreamReader(ZCNameFile);
			do
			{
				string? text = streamReader.ReadLine();
				int num = int.Parse(text!.Substring(0, 4));
				string text2 = text!.Substring(5).Trim();
				int num2 = text2.IndexOf("=");
				int num3 = text2.IndexOf("=", num2 + 2);
				if (num3 > num2)
				{
					text2 = text2.Substring(0, num3 - 1);
				}
				int result;
				switch (ListType)
				{
				case 0:
					if (num2 > 0 && !int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 1:
					if (num2 > 0)
					{
						if (!int.TryParse(text2.Substring(num2 + 1, 2), out result))
						{
							int index = cmbNames.get_Items().Add((object)text2.Substring(num2 + 1));
							StarList.Insert(index, num);
						}
					}
					else if (!int.TryParse(text2.Substring(0, 2), out result))
					{
						int index = cmbNames.get_Items().Add((object)text2.Trim());
						StarList.Insert(index, num);
					}
					break;
				case 2:
				{
					int index = cmbNames.get_Items().Add((object)text2);
					StarList.Insert(index, num);
					break;
				}
				case 3:
					if (ZCMagnitude(num) < 2.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 4:
					if (ZCMagnitude(num) < 3.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				case 5:
					if (ZCMagnitude(num) < 4.0)
					{
						int index = cmbNames.get_Items().Add((object)text2);
						StarList.Insert(index, num);
					}
					break;
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			Cursor.set_Current(Cursors.get_Default());
			((ListControl)cmbNames).set_SelectedIndex(1);
		}

		private double ZCMagnitude(int ZC)
		{
			XZ80Q.Get_ZC_Star(ZC);
			return XZ80Q.Mv;
		}

		private void cmbNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				XZ80Q.Get_ZC_Star((int)StarList[((ListControl)cmbNames).get_SelectedIndex()]);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				ChangingStar = false;
			}
		}

		private void SetcmbNames(int ZC)
		{
			for (int i = 1; i < cmbNames.get_Items().get_Count(); i++)
			{
				if (ZC == (int)StarList[i])
				{
					((ListControl)cmbNames).set_SelectedIndex(i);
					return;
				}
			}
			((ListControl)cmbNames).set_SelectedIndex(0);
		}

		private void txtZC_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtZC).get_Text(), out var result))
				{
					result = 0;
				}
				XZ80Q.Get_ZC_Star(result);
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtSAO_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtSAO).get_Text(), out var result))
				{
					result = 0;
				}
				if (XZ80Q.Get_SAO_Star(result))
				{
					((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
					((Control)txtXZ).set_Text(XZ80Q.XZ.ToString());
				}
				else
				{
					TextBox obj = txtZC;
					string text;
					((Control)txtXZ).set_Text(text = "0");
					((Control)obj).set_Text(text);
				}
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void txtXZ_TextChanged(object sender, EventArgs e)
		{
			if (!ChangingStar)
			{
				ChangingStar = true;
				if (!int.TryParse(((Control)txtXZ).get_Text(), out var result))
				{
					result = 0;
				}
				XZ80Q.Get_XZ_Star(result);
				((Control)txtZC).set_Text(XZ80Q.ZC.ToString());
				((Control)txtSAO).set_Text(XZ80Q.SAO.ToString());
				SetcmbNames(XZ80Q.ZC);
				ChangingStar = false;
			}
		}

		private void cmdFindStar_Click(object sender, EventArgs e)
		{
			SearchType = 0;
			Search(0);
		}

		private void cmdFindPlanet_Click(object sender, EventArgs e)
		{
			SearchType = 1;
			Search(1);
		}

		private void cmdFindAsteroid_Click(object sender, EventArgs e)
		{
			SearchType = 2;
			Search(2);
		}

		private void cmdFindObject_Click(object sender, EventArgs e)
		{
			SearchType = 3;
			Search(3);
		}

		private void Search(int SearchType)
		{
			//IL_010b: Unknown result type (might be due to invalid IL or missing references)
			//IL_019e: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cb: Unknown result type (might be due to invalid IL or missing references)
			int Year = 0;
			int num = 0;
			double num2 = 0.0;
			double num3 = 5.0;
			double EpochMeanAnomaly = 0.0;
			double a = 0.0;
			double num4 = 0.0;
			double e = 0.0;
			double i = 0.0;
			double node = 0.0;
			double perihelion = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double OsculatingDate = 0.0;
			double perihelionDate = 0.0;
			int num7 = 0;
			double Longitude = 0.0;
			double Latitude = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			AsteroidElements asteroidElements = new AsteroidElements();
			List<AsteroidElements> elementsAt1Day = new List<AsteroidElements>();
			LunarAsteroidXYZ lunarAsteroidXYZ = new LunarAsteroidXYZ();
			int num10 = (int)updnStartYear.get_Value();
			int num11 = (int)updnEndYear.get_Value();
			if (num11 < num10)
			{
				MessageBox.Show("The End year must be greater than the Start year", "Invald Year range", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			double num12 = Utilities.JD_from_Date(num10, 1, 1.0);
			double num13 = Utilities.JD_from_Date(num11 + 1, 1, 1.0);
			lstEvents.get_Items().Clear();
			JDList.Clear();
			switch (SearchType)
			{
			case 0:
				if (!int.TryParse(((Control)txtXZ).get_Text(), out StarNo))
				{
					StarNo = 0;
				}
				if ((StarNo < 1) | (StarNo > 244437))
				{
					MessageBox.Show("The XZ number must be in the range 1 to 244437", "Invald XZ number", (MessageBoxButtons)0, (MessageBoxIcon)48);
					return;
				}
				ObjectID = "ZC" + ((Control)txtZC).get_Text().Trim();
				if (ObjectID == "ZC")
				{
					ObjectID = "SAO" + ((Control)txtSAO).get_Text().Trim();
				}
				if (ObjectID == "SAO")
				{
					ObjectID = "XZ" + ((Control)txtXZ).get_Text().Trim();
				}
				break;
			case 1:
				if (optMercury.get_Checked())
				{
					PlanetNumber = 1;
				}
				else if (optVenus.get_Checked())
				{
					PlanetNumber = 2;
				}
				else if (optMars.get_Checked())
				{
					PlanetNumber = 4;
				}
				else if (optJupiter.get_Checked())
				{
					PlanetNumber = 5;
				}
				else if (optSaturn.get_Checked())
				{
					PlanetNumber = 6;
				}
				else if (optUranus.get_Checked())
				{
					PlanetNumber = 7;
				}
				else if (optNeptune.get_Checked())
				{
					PlanetNumber = 8;
				}
				else
				{
					PlanetNumber = 9;
				}
				ObjectID = Utilities.Planets[PlanetNumber];
				break;
			case 2:
			{
				asteroidElements = Elements.UserAsteroids_Lunar.AstElements[((ListControl)cmbAsteroid).get_SelectedIndex()];
				ObjectID = asteroidElements.IDName.Trim();
				bool flag = true;
				int num14 = (int)Math.Floor(num12 / 10000.0);
				int num15 = (int)Math.Floor(num13 / 10000.0);
				if (num14 > 245)
				{
					num14 = 245;
				}
				if (num15 < 245)
				{
					num15 = 245;
				}
				for (int j = num14; j <= num15; j++)
				{
					flag &= File.Exists(AppPath + "\\Resource Files\\" + Convert.ToString(j) + "0000_XYZ.bin");
				}
				if (!flag)
				{
					((Control)lunarAsteroidXYZ).set_Text("Provide planetary coordinates to integrate the orbits of asteroids to " + Utilities.Date_from_JD(num13, 0));
					((Form)lunarAsteroidXYZ).ShowDialog();
				}
				flag = true;
				for (int k = num14; k <= num15; k++)
				{
					flag &= File.Exists(AppPath + "\\Resource Files\\" + Convert.ToString(k) + "0000_XYZ.bin");
				}
				if (!flag)
				{
					return;
				}
				break;
			}
			default:
				ObjectID = "an Object";
				break;
			}
			double num16 = num12;
			pbarFind.set_Minimum(0);
			pbarFind.set_Maximum((int)(num13 - num12 + 10.0));
			((Control)pbarFind).set_Visible(true);
			do
			{
				Utilities.Date_from_JD(num16, out Year, out var _, out var day);
				pbarFind.set_Value((int)(num16 - num12));
				Application.DoEvents();
				if (SearchType != 1 && Year != num)
				{
					num = Year;
					num2 = num16 + 182.0 - day;
					switch (SearchType)
					{
					case 0:
						XZ80Q.Get_XZ_Star(StarNo);
						RAStar = XZ80Q.RA_rad;
						DecStar = XZ80Q.Dec_rad;
						num8 = XZ80Q.PMRA_rad;
						num9 = XZ80Q.PMDec_rad;
						Utilities.ApparentStarPosition(ref RAStar, ref DecStar, num8, num9, 0, num2, use2006values_Not1976: false);
						break;
					case 2:
					{
						num7 = asteroidElements.IDNumber;
						string iDName = asteroidElements.IDName;
						EpochMeanAnomaly = asteroidElements.Meananomaly / (180.0 / Math.PI);
						perihelion = asteroidElements.Perihelion / (180.0 / Math.PI);
						node = asteroidElements.Node / (180.0 / Math.PI);
						i = asteroidElements.I / (180.0 / Math.PI);
						e = asteroidElements.E;
						a = asteroidElements.A;
						num4 = asteroidElements.q;
						num5 = asteroidElements.H0;
						num6 = asteroidElements.G_phaseCoeff;
						num3 = asteroidElements.LogR_Coeff;
						double diameter_Mean = asteroidElements.Diameter_Mean;
						string dia_Source = asteroidElements.Dia_Source;
						double peakEphemUncert = asteroidElements.PeakEphemUncert;
						OsculatingDate = Utilities.JD_from_Date(asteroidElements.EpochYear, asteroidElements.EpochMonth, asteroidElements.EpochDay);
						if (asteroidElements.Meananomaly == 0.0)
						{
							perihelionDate = OsculatingDate;
							OsculatingDate = 0.0;
						}
						else
						{
							perihelionDate = 0.0;
							Utilities.NumericIntegrate(Math.Floor(num2) + 0.5, ref OsculatingDate, ref EpochMeanAnomaly, ref a, ref num4, ref e, ref i, ref node, ref perihelion, saveflag: false, num7, iDName, num5, num6, num3, diameter_Mean, dia_Source, peakEphemUncert, 0, 0, "", "", "", 0.0, 0.0, 0.0, elementsAt1Day);
						}
						break;
					}
					default:
					{
						if (!int.TryParse(((Control)txtRH1).get_Text(), out var result) || !int.TryParse(((Control)txtRM1).get_Text(), out var result2) || !double.TryParse(((Control)txtRS1).get_Text(), out var result3) || !int.TryParse(((Control)txtDD1).get_Text(), out var result4) || !int.TryParse(((Control)txtDM1).get_Text(), out var result5) || !double.TryParse(((Control)txtDS1).get_Text(), out var result6))
						{
							return;
						}
						RAStar2000 = (RAStar = ((double)result + (double)result2 / 60.0 + result3 / 3600.0) * 15.0 / (180.0 / Math.PI));
						DecStar = ((double)Math.Abs(result4) + (double)result5 / 60.0 + result6 / 3600.0) / (180.0 / Math.PI);
						if (((Control)txtDD1).get_Text().Contains("-"))
						{
							DecStar = 0.0 - DecStar;
						}
						DecStar2000 = DecStar;
						num8 = 0.0;
						num9 = 0.0;
						Utilities.ApparentStarPosition(ref RAStar, ref DecStar, num8, num9, 0, num2, use2006values_Not1976: false);
						break;
					}
					}
				}
				double num17 = 0.0;
				Utilities.Nutation(num16, out var _, out var _, out var TrueEcliptic);
				double RA;
				double Dec;
				double Parallax;
				double MoonLongitude;
				double MoonLatitude;
				double num18;
				do
				{
					double RadiusVector;
					if (SearchType == 1)
					{
						Utilities.PlanetGeocentric(num16 + num17, PlanetNumber, 1E-06, 0, out RAStar, out DecStar, out RadiusVector);
					}
					if (SearchType == 2)
					{
						double AstrometricGeocentricDistance;
						double Magnitude;
						double Elongation;
						double PhaseAngle;
						if (e < 0.97)
						{
							Utilities.PositionfromElements(num16 + num17, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, EpochMeanAnomaly, a * (1.0 - e), e, perihelion, node, i, num5, num6, num3, 1E-05, out RAStar, out DecStar, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle);
						}
						else
						{
							Utilities.PositionfromElements(num16 + num17, 0.0, 0.0, 0.0, perihelionDate, OsculatingDate, 0.0, a, e, perihelion, node, i, num5, num6, num3, 1E-05, out RAStar, out DecStar, out RadiusVector, out AstrometricGeocentricDistance, out Magnitude, out Elongation, out PhaseAngle);
						}
						Utilities.ApparentStarPosition(ref RAStar, ref DecStar, 0.0, 0.0, 0, num2, use2006values_Not1976: false);
					}
					Utilities.RA_DEC_to_Long_Lat(RAStar, DecStar, TrueEcliptic, ref Longitude, ref Latitude);
					Utilities.QuickMoon(num16 + num17, out RA, out Dec, out Parallax, out MoonLongitude, out MoonLatitude);
					num18 = Longitude - MoonLongitude;
					if (num18 < -Math.PI)
					{
						num18 += Math.PI * 2.0;
					}
					if (num18 > Math.PI)
					{
						num18 -= Math.PI * 2.0;
					}
					num17 += Math.Floor(num18 / 0.22);
				}
				while (Math.Abs(num18) > 0.28);
				Utilities.QuickMoon(num16 + num17, out RA, out Dec, out Parallax, out MoonLongitude, out MoonLatitude);
				Utilities.QuickMoon(num16 + num17 + 1.0, out var RA2, out var _, out Parallax, out var MoonLongitude2, out var MoonLatitude2);
				num18 = Longitude - MoonLongitude;
				if (num18 < -Math.PI)
				{
					num18 += Math.PI * 2.0;
				}
				if (num18 > Math.PI)
				{
					num18 -= Math.PI * 2.0;
				}
				double num19 = Longitude - MoonLongitude2;
				if (num19 < -Math.PI)
				{
					num19 += Math.PI * 2.0;
				}
				if (num19 > Math.PI)
				{
					num19 -= Math.PI * 2.0;
				}
				if (num19 - num18 > Math.PI)
				{
					num18 -= Math.PI * 2.0;
				}
				if (num19 - num18 < -Math.PI)
				{
					num19 += Math.PI * 2.0;
				}
				double num20 = Latitude - (MoonLatitude - num18 / (num19 - num18) * (MoonLatitude2 - MoonLatitude));
				if (Math.Abs(num20) < 1.35 * Parallax)
				{
					double num21 = RAStar - RA;
					if (num21 < -Math.PI)
					{
						num21 += Math.PI * 2.0;
					}
					if (num21 > Math.PI)
					{
						num21 -= Math.PI * 2.0;
					}
					double num22 = RAStar - RA2;
					if (num22 < -Math.PI)
					{
						num22 += Math.PI * 2.0;
					}
					if (num22 > Math.PI)
					{
						num22 -= Math.PI * 2.0;
					}
					string text = ((!(num20 > 0.0)) ? " n" : " s");
					double num23 = num21 / (num21 - num22);
					lstEvents.get_Items().Add((object)(Utilities.Date_from_JD(num16 + num17 + num23, 1) + text));
					JDList.Add(num16 + num17);
				}
				num16 = num16 + num17 + 25.0;
			}
			while (num16 < num13 + 5.0);
			((Control)pbarFind).set_Visible(false);
			Button obj = cmdPlotAll;
			bool visible;
			((Control)cmdCancel).set_Visible(visible = lstEvents.get_Items().get_Count() > 0);
			((Control)obj).set_Visible(visible);
		}

		private void lstEvents_DoubleClick(object sender, EventArgs e)
		{
			LunarOccultations.LunarPredictionsShowForm();
			((Form)LunarOccultations.LunarPrediction).set_WindowState((FormWindowState)1);
			Compute_lstEvent(((ListControl)lstEvents).get_SelectedIndex(), SingleEvent: true);
		}

		private void Compute_lstEvent(int EventID, bool SingleEvent)
		{
			LunarOccultations.InitialiseSearch();
			double num = (double)JDList[EventID];
			for (int i = 0; i <= 2; i++)
			{
				int num2 = i;
				if (i == 2)
				{
					num2 = -1;
				}
				LunarOccultations.SetPredictionDatesExternally(num + (double)num2);
				LunarOccultationPrediction.CurrentElementsAreValid = true;
				LunarOccultations.NewDate(num + (double)num2, 0, SiteSpecific: false, 90.0, -90.0, 180.0, -180.0);
				if (SearchType == 0)
				{
					LunarOccultations.OneOffStar(StarNo, useCoordinates: false, 0.0, 0.0, 0.0, " ");
				}
				if (SearchType == 1)
				{
					LunarOccultations.OneOffPlanet(PlanetNumber, 0, useAsteroid: false);
				}
				if (SearchType == 2)
				{
					if (((ListControl)cmbAsteroid).get_SelectedIndex() < 0)
					{
						return;
					}
					AsteroidNumber = int.Parse(cmbAsteroid.get_Items().get_Item(((ListControl)cmbAsteroid).get_SelectedIndex()).ToString()!.Substring(0, 7));
					LunarOccultations.OneOffPlanet(0, AsteroidNumber, useAsteroid: true);
				}
				if (SearchType == 3)
				{
					LunarOccultations.OneOffStar(StarNo, useCoordinates: true, RAStar2000, DecStar2000, (double)updnMag.get_Value(), ((Control)txtLabel).get_Text());
				}
				if (LunarOccultations.Elements.Count > 0)
				{
					break;
				}
			}
			if (SingleEvent)
			{
				LunarOccultations.ShowOccultationWorld();
			}
		}

		private void Lunar_FindAnOccultation_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Width() < 500) | (((Control)this).get_Height() < 500)))
			{
				((Control)lstEvents).set_Width(((Control)this).get_Width() - 31);
				((Control)lstEvents).set_Height(((Control)this).get_Height() - 490);
			}
		}

		private void cmdPlotAll_Click(object sender, EventArgs e)
		{
			Plot_All_Listed();
		}

		private void Plot_All_Listed()
		{
			((Control)cmdPlotAll).set_Visible(false);
			Cancel = false;
			LunarOccultations.PlotAll_Prepare();
			for (int i = 0; i < lstEvents.get_Items().get_Count(); i++)
			{
				((ListControl)lstEvents).set_SelectedIndex(i);
				Compute_lstEvent(i, SingleEvent: false);
				LunarOccultations.PlotAll_Plot(i, optBW.get_Checked());
				Application.DoEvents();
				if (Cancel)
				{
					break;
				}
			}
			((Control)cmdPlotAll).set_Visible(true);
		}

		private void plotAsWorldMapToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Compute_lstEvent(((ListControl)lstEvents).get_SelectedIndex(), SingleEvent: true);
		}

		private void plotALLListedEventsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Plot_All_Listed();
		}

		private void viewInGoogleEarthToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Compute_lstEvent(((ListControl)lstEvents).get_SelectedIndex(), SingleEvent: false);
			string text = LunarOccultations.EventID(0);
			LunarOccultations.CreateGoogleEarthKMZFile_ForWorld(text, "Occultation of " + text, AutoOpenFile: true, View: true, "");
		}

		private void saveGoogleEarthKMLFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Compute_lstEvent(((ListControl)lstEvents).get_SelectedIndex(), SingleEvent: false);
			string text = LunarOccultations.EventID(0);
			LunarOccultations.CreateGoogleEarthKMZFile_ForWorld(text, "Occultation of " + text, AutoOpenFile: false, View: false, "");
		}

		private void saveGoogleMapHTMFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Compute_lstEvent(((ListControl)lstEvents).get_SelectedIndex(), SingleEvent: false);
			string text = LunarOccultations.EventID(0);
			LunarOccultations.CreateGoogleMapHTMFile_ForWorld(text, "Occultation of " + text, AutoOpenFile: false);
		}

		private void lstEvents_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_001f: Invalid comparison between Unknown and I4
			if (lstEvents.get_Items().get_Count() >= 1 && (int)e.get_Button() == 2097152)
			{
				int num = lstEvents.IndexFromPoint(e.get_X(), e.get_Y());
				if (num >= 0 && num < lstEvents.get_Items().get_Count())
				{
					((ListControl)lstEvents).set_SelectedIndex(num);
				}
				((Control)lstEvents).Refresh();
			}
		}

		private string CollectEvents()
		{
			EventLabel = "Occultations of " + ObjectID + " " + updnStartYear.get_Value() + " to " + updnEndYear.get_Value();
			if (lstEvents.get_Items().get_Count() < 0)
			{
				return "";
			}
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstEvents.get_Items().get_Count();
			stringBuilder.AppendLine(EventLabel);
			stringBuilder.AppendLine("");
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstEvents.get_Items().get_Item(i).ToString());
				if ((i % 5 == 4) & Settings.Default.Skip5thLine)
				{
					stringBuilder.AppendLine("");
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectEvents());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectEvents());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_LunarPredictions = Output.SaveAppendPredictionText(CollectEvents(), EventLabel, Settings.Default.Save_LunarPredictions);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			((Control)cmdFindAsteroid).set_Enabled(((int)updnStartYear.get_Value() > 1920) & ((int)updnEndYear.get_Value() < 2050));
		}

		private void updnEndYear_ValueChanged(object sender, EventArgs e)
		{
			((Control)cmdFindAsteroid).set_Enabled(((int)updnStartYear.get_Value() > 1920) & ((int)updnEndYear.get_Value() < 2050));
		}

		private void hToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Find occultations of a particular object");
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			Cancel = true;
		}

		private void cmbClusters_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)cmbClusters).get_SelectedIndex() == 0)
			{
				TextBox obj = txtRH1;
				TextBox obj2 = txtRM1;
				TextBox obj3 = txtRS1;
				TextBox obj4 = txtDD1;
				TextBox obj5 = txtDM1;
				string text;
				((Control)txtDS1).set_Text(text = "0");
				string text2;
				((Control)obj5).set_Text(text2 = text);
				string text3;
				((Control)obj4).set_Text(text3 = text2);
				string text4;
				((Control)obj3).set_Text(text4 = text3);
				string text5;
				((Control)obj2).set_Text(text5 = text4);
				((Control)obj).set_Text(text5);
				updnMag.set_Value(6m);
				((Control)txtLabel).set_Text("");
			}
			else
			{
				((Control)txtRH1).set_Text(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(9, 2).Trim());
				((Control)txtRM1).set_Text(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(12, 2).Trim());
				((Control)txtRS1).set_Text(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(15, 2).Trim());
				((Control)txtDD1).set_Text(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(18, 3).Trim());
				((Control)txtDM1).set_Text(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(22, 2).Trim());
				((Control)txtDS1).set_Text(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(25, 2).Trim());
				updnMag.set_Value(Convert.ToDecimal(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(28)));
				((Control)txtLabel).set_Text(OpenClusters[((ListControl)cmbClusters).get_SelectedIndex()].Substring(0, 8).Trim());
			}
		}

		private void updnStartYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 10);
		}

		private void updnEndYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 10);
		}

		private void updnStartYear_MouseClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 10);
		}

		private void updnEndYear_MouseClick(object sender, MouseEventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 10);
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
			//IL_000c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0016: Expected O, but got Unknown
			//IL_001d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Expected O, but got Unknown
			//IL_0028: Unknown result type (might be due to invalid IL or missing references)
			//IL_0032: Expected O, but got Unknown
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			//IL_003d: Expected O, but got Unknown
			//IL_003e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0048: Expected O, but got Unknown
			//IL_0049: Unknown result type (might be due to invalid IL or missing references)
			//IL_0053: Expected O, but got Unknown
			//IL_0054: Unknown result type (might be due to invalid IL or missing references)
			//IL_005e: Expected O, but got Unknown
			//IL_005f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Expected O, but got Unknown
			//IL_006a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0074: Expected O, but got Unknown
			//IL_0075: Unknown result type (might be due to invalid IL or missing references)
			//IL_007f: Expected O, but got Unknown
			//IL_0080: Unknown result type (might be due to invalid IL or missing references)
			//IL_008a: Expected O, but got Unknown
			//IL_008b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0095: Expected O, but got Unknown
			//IL_0096: Unknown result type (might be due to invalid IL or missing references)
			//IL_00a0: Expected O, but got Unknown
			//IL_00a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ab: Expected O, but got Unknown
			//IL_00ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_00b6: Expected O, but got Unknown
			//IL_00b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c1: Expected O, but got Unknown
			//IL_00c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Expected O, but got Unknown
			//IL_00cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_00d7: Expected O, but got Unknown
			//IL_00d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e2: Expected O, but got Unknown
			//IL_00e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ed: Expected O, but got Unknown
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f8: Expected O, but got Unknown
			//IL_00f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0103: Expected O, but got Unknown
			//IL_0104: Unknown result type (might be due to invalid IL or missing references)
			//IL_010e: Expected O, but got Unknown
			//IL_010f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0119: Expected O, but got Unknown
			//IL_011a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0124: Expected O, but got Unknown
			//IL_0125: Unknown result type (might be due to invalid IL or missing references)
			//IL_012f: Expected O, but got Unknown
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_013a: Expected O, but got Unknown
			//IL_013b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0145: Expected O, but got Unknown
			//IL_0146: Unknown result type (might be due to invalid IL or missing references)
			//IL_0150: Expected O, but got Unknown
			//IL_0151: Unknown result type (might be due to invalid IL or missing references)
			//IL_015b: Expected O, but got Unknown
			//IL_015c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0166: Expected O, but got Unknown
			//IL_0167: Unknown result type (might be due to invalid IL or missing references)
			//IL_0171: Expected O, but got Unknown
			//IL_0172: Unknown result type (might be due to invalid IL or missing references)
			//IL_017c: Expected O, but got Unknown
			//IL_017d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0187: Expected O, but got Unknown
			//IL_0188: Unknown result type (might be due to invalid IL or missing references)
			//IL_0192: Expected O, but got Unknown
			//IL_0193: Unknown result type (might be due to invalid IL or missing references)
			//IL_019d: Expected O, but got Unknown
			//IL_019e: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a8: Expected O, but got Unknown
			//IL_01a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b3: Expected O, but got Unknown
			//IL_01b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01be: Expected O, but got Unknown
			//IL_01bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c9: Expected O, but got Unknown
			//IL_01ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d4: Expected O, but got Unknown
			//IL_01d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01df: Expected O, but got Unknown
			//IL_01e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ea: Expected O, but got Unknown
			//IL_01eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f5: Expected O, but got Unknown
			//IL_01f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0200: Expected O, but got Unknown
			//IL_0201: Unknown result type (might be due to invalid IL or missing references)
			//IL_020b: Expected O, but got Unknown
			//IL_020c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0216: Expected O, but got Unknown
			//IL_0217: Unknown result type (might be due to invalid IL or missing references)
			//IL_0221: Expected O, but got Unknown
			//IL_0222: Unknown result type (might be due to invalid IL or missing references)
			//IL_022c: Expected O, but got Unknown
			//IL_022d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0237: Expected O, but got Unknown
			//IL_0238: Unknown result type (might be due to invalid IL or missing references)
			//IL_0242: Expected O, but got Unknown
			//IL_0243: Unknown result type (might be due to invalid IL or missing references)
			//IL_024d: Expected O, but got Unknown
			//IL_024e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0258: Expected O, but got Unknown
			//IL_0259: Unknown result type (might be due to invalid IL or missing references)
			//IL_0263: Expected O, but got Unknown
			//IL_0264: Unknown result type (might be due to invalid IL or missing references)
			//IL_026e: Expected O, but got Unknown
			//IL_026f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0279: Expected O, but got Unknown
			//IL_027a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0284: Expected O, but got Unknown
			//IL_0285: Unknown result type (might be due to invalid IL or missing references)
			//IL_028f: Expected O, but got Unknown
			//IL_0290: Unknown result type (might be due to invalid IL or missing references)
			//IL_029a: Expected O, but got Unknown
			//IL_029b: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a5: Expected O, but got Unknown
			//IL_02a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b0: Expected O, but got Unknown
			//IL_02b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02bb: Expected O, but got Unknown
			//IL_02bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c6: Expected O, but got Unknown
			//IL_02c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d1: Expected O, but got Unknown
			//IL_02d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02dc: Expected O, but got Unknown
			//IL_02dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e7: Expected O, but got Unknown
			//IL_02e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f2: Expected O, but got Unknown
			//IL_02f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fd: Expected O, but got Unknown
			//IL_02fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0308: Expected O, but got Unknown
			//IL_0309: Unknown result type (might be due to invalid IL or missing references)
			//IL_0313: Expected O, but got Unknown
			//IL_0314: Unknown result type (might be due to invalid IL or missing references)
			//IL_031e: Expected O, but got Unknown
			//IL_031f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0329: Expected O, but got Unknown
			//IL_032a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0334: Expected O, but got Unknown
			//IL_0335: Unknown result type (might be due to invalid IL or missing references)
			//IL_033f: Expected O, but got Unknown
			//IL_0340: Unknown result type (might be due to invalid IL or missing references)
			//IL_034a: Expected O, but got Unknown
			//IL_034b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0355: Expected O, but got Unknown
			//IL_0356: Unknown result type (might be due to invalid IL or missing references)
			//IL_0360: Expected O, but got Unknown
			//IL_0361: Unknown result type (might be due to invalid IL or missing references)
			//IL_036b: Expected O, but got Unknown
			//IL_036c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0376: Expected O, but got Unknown
			//IL_04b9: Unknown result type (might be due to invalid IL or missing references)
			//IL_04c3: Expected O, but got Unknown
			//IL_238b: Unknown result type (might be due to invalid IL or missing references)
			//IL_2395: Expected O, but got Unknown
			//IL_2485: Unknown result type (might be due to invalid IL or missing references)
			//IL_248f: Expected O, but got Unknown
			//IL_2da6: Unknown result type (might be due to invalid IL or missing references)
			//IL_2db0: Expected O, but got Unknown
			components = new Container();
			lstEvents = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			plotAsWorldMapToolStripMenuItem = new ToolStripMenuItem();
			viewInGoogleEarthToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleEarthKMLFileToolStripMenuItem = new ToolStripMenuItem();
			saveGoogleMapHTMFileToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			plotALLListedEventsToolStripMenuItem = new ToolStripMenuItem();
			grpStars = new GroupBox();
			cmdFindStar = new Button();
			groupBox2 = new GroupBox();
			label3 = new Label();
			label2 = new Label();
			label1 = new Label();
			txtXZ = new TextBox();
			txtSAO = new TextBox();
			txtZC = new TextBox();
			groupBox1 = new GroupBox();
			optMag4 = new RadioButton();
			optMag3 = new RadioButton();
			optMag2 = new RadioButton();
			optAll = new RadioButton();
			optBayer = new RadioButton();
			optProper = new RadioButton();
			cmbNames = new ComboBox();
			grpPlanets = new GroupBox();
			cmdFindPlanet = new Button();
			optPluto = new RadioButton();
			optNeptune = new RadioButton();
			optUranus = new RadioButton();
			optSaturn = new RadioButton();
			optJupiter = new RadioButton();
			optMars = new RadioButton();
			optVenus = new RadioButton();
			optMercury = new RadioButton();
			grpAsteroids = new GroupBox();
			label16 = new Label();
			cmdFindAsteroid = new Button();
			cmbAsteroid = new ComboBox();
			grpCoordinates = new GroupBox();
			label18 = new Label();
			label11 = new Label();
			txtLabel = new TextBox();
			cmbClusters = new ComboBox();
			label10 = new Label();
			updnMag = new NumericUpDown();
			label15 = new Label();
			label7 = new Label();
			txtDS1 = new TextBox();
			txtDM1 = new TextBox();
			txtDD1 = new TextBox();
			label8 = new Label();
			label9 = new Label();
			label13 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			txtRS1 = new TextBox();
			txtRM1 = new TextBox();
			txtRH1 = new TextBox();
			cmdFindObject = new Button();
			updnStartYear = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			label12 = new Label();
			label14 = new Label();
			pbarFind = new ProgressBar();
			cmdPlotAll = new Button();
			label17 = new Label();
			menuStrip1 = new MenuStrip();
			withListOfEventsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			hToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdCancel = new Button();
			optColour = new RadioButton();
			optBW = new RadioButton();
			((Control)contextMenuStrip1).SuspendLayout();
			((Control)grpStars).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)grpPlanets).SuspendLayout();
			((Control)grpAsteroids).SuspendLayout();
			((Control)grpCoordinates).SuspendLayout();
			((ISupportInitialize)updnMag).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstEvents).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstEvents).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEvents).set_FormattingEnabled(true);
			lstEvents.set_ItemHeight(14);
			((Control)lstEvents).set_Location(new Point(12, 491));
			lstEvents.set_MultiColumn(true);
			((Control)lstEvents).set_Name("lstEvents");
			((Control)lstEvents).set_Size(new Size(679, 172));
			((Control)lstEvents).set_TabIndex(0);
			((Control)lstEvents).add_DoubleClick((EventHandler)lstEvents_DoubleClick);
			((Control)lstEvents).add_MouseDown(new MouseEventHandler(lstEvents_MouseDown));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[6]
			{
				(ToolStripItem)plotAsWorldMapToolStripMenuItem,
				(ToolStripItem)viewInGoogleEarthToolStripMenuItem,
				(ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem,
				(ToolStripItem)saveGoogleMapHTMFileToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)plotALLListedEventsToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(213, 120));
			((ToolStripItem)plotAsWorldMapToolStripMenuItem).set_Image((Image)Resources.EARTH);
			((ToolStripItem)plotAsWorldMapToolStripMenuItem).set_Name("plotAsWorldMapToolStripMenuItem");
			((ToolStripItem)plotAsWorldMapToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)plotAsWorldMapToolStripMenuItem).set_Text("Plot as world map");
			((ToolStripItem)plotAsWorldMapToolStripMenuItem).add_Click((EventHandler)plotAsWorldMapToolStripMenuItem_Click);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Name("viewInGoogleEarthToolStripMenuItem");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).set_Text("View in GoogleEarth");
			((ToolStripItem)viewInGoogleEarthToolStripMenuItem).add_Click((EventHandler)viewInGoogleEarthToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Name("saveGoogleEarthKMLFileToolStripMenuItem");
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).set_Text("Save GoogleEarth KML file");
			((ToolStripItem)saveGoogleEarthKMLFileToolStripMenuItem).add_Click((EventHandler)saveGoogleEarthKMLFileToolStripMenuItem_Click);
			((ToolStripItem)saveGoogleMapHTMFileToolStripMenuItem).set_Image((Image)Resources.mm_20_red);
			((ToolStripItem)saveGoogleMapHTMFileToolStripMenuItem).set_Name("saveGoogleMapHTMFileToolStripMenuItem");
			((ToolStripItem)saveGoogleMapHTMFileToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)saveGoogleMapHTMFileToolStripMenuItem).set_Text("Save GoogleMap HTM file");
			((ToolStripItem)saveGoogleMapHTMFileToolStripMenuItem).add_Click((EventHandler)saveGoogleMapHTMFileToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(209, 6));
			((ToolStripItem)plotALLListedEventsToolStripMenuItem).set_Font(new Font("Tahoma", 8.25f, FontStyle.Bold));
			((ToolStripItem)plotALLListedEventsToolStripMenuItem).set_Name("plotALLListedEventsToolStripMenuItem");
			((ToolStripItem)plotALLListedEventsToolStripMenuItem).set_Size(new Size(212, 22));
			((ToolStripItem)plotALLListedEventsToolStripMenuItem).set_Text("Plot ALL listed events");
			((ToolStripItem)plotALLListedEventsToolStripMenuItem).add_Click((EventHandler)plotALLListedEventsToolStripMenuItem_Click);
			((Control)grpStars).set_Anchor((AnchorStyles)1);
			((Control)grpStars).get_Controls().Add((Control)(object)cmdFindStar);
			((Control)grpStars).get_Controls().Add((Control)(object)groupBox2);
			((Control)grpStars).get_Controls().Add((Control)(object)groupBox1);
			((Control)grpStars).set_Location(new Point(60, 63));
			((Control)grpStars).set_Name("grpStars");
			((Control)grpStars).set_Size(new Size(582, 118));
			((Control)grpStars).set_TabIndex(1);
			grpStars.set_TabStop(false);
			((Control)grpStars).set_Text("Stars");
			((Control)cmdFindStar).set_Location(new Point(485, 40));
			((Control)cmdFindStar).set_Name("cmdFindStar");
			((Control)cmdFindStar).set_Size(new Size(91, 46));
			((Control)cmdFindStar).set_TabIndex(12);
			((Control)cmdFindStar).set_Text("Find Star");
			((ButtonBase)cmdFindStar).set_UseVisualStyleBackColor(true);
			((Control)cmdFindStar).add_Click((EventHandler)cmdFindStar_Click);
			((Control)groupBox2).get_Controls().Add((Control)(object)label3);
			((Control)groupBox2).get_Controls().Add((Control)(object)label2);
			((Control)groupBox2).get_Controls().Add((Control)(object)label1);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtXZ);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtSAO);
			((Control)groupBox2).get_Controls().Add((Control)(object)txtZC);
			((Control)groupBox2).set_Location(new Point(346, 12));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(100, 103));
			((Control)groupBox2).set_TabIndex(11);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("by Number");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(13, 79));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(23, 13));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text("XZ");
			label3.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(5, 53));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(32, 13));
			((Control)label2).set_TabIndex(5);
			((Control)label2).set_Text("SAO");
			label2.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(13, 27));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(23, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("ZC");
			label1.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtXZ).set_Location(new Point(39, 75));
			((Control)txtXZ).set_Name("txtXZ");
			((Control)txtXZ).set_Size(new Size(48, 20));
			((Control)txtXZ).set_TabIndex(3);
			((Control)txtXZ).add_TextChanged((EventHandler)txtXZ_TextChanged);
			((Control)txtSAO).set_Location(new Point(39, 49));
			((Control)txtSAO).set_Name("txtSAO");
			((Control)txtSAO).set_Size(new Size(48, 20));
			((Control)txtSAO).set_TabIndex(2);
			((Control)txtSAO).add_TextChanged((EventHandler)txtSAO_TextChanged);
			((Control)txtZC).set_Location(new Point(39, 23));
			((Control)txtZC).set_Name("txtZC");
			((Control)txtZC).set_Size(new Size(48, 20));
			((Control)txtZC).set_TabIndex(1);
			((Control)txtZC).add_TextChanged((EventHandler)txtZC_TextChanged);
			((Control)groupBox1).get_Controls().Add((Control)(object)optMag4);
			((Control)groupBox1).get_Controls().Add((Control)(object)optMag3);
			((Control)groupBox1).get_Controls().Add((Control)(object)optMag2);
			((Control)groupBox1).get_Controls().Add((Control)(object)optAll);
			((Control)groupBox1).get_Controls().Add((Control)(object)optBayer);
			((Control)groupBox1).get_Controls().Add((Control)(object)optProper);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmbNames);
			((Control)groupBox1).set_Location(new Point(27, 12));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(309, 103));
			((Control)groupBox1).set_TabIndex(10);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("by Name");
			((Control)optMag4).set_AutoSize(true);
			((Control)optMag4).set_Location(new Point(130, 57));
			((Control)optMag4).set_Name("optMag4");
			((Control)optMag4).set_Size(new Size(155, 17));
			((Control)optMag4).set_TabIndex(9);
			((Control)optMag4).set_Text("Magnitude brighter than 4.0");
			((ButtonBase)optMag4).set_UseVisualStyleBackColor(true);
			optMag4.add_CheckedChanged((EventHandler)optMag4_CheckedChanged);
			((Control)optMag3).set_AutoSize(true);
			((Control)optMag3).set_Location(new Point(130, 36));
			((Control)optMag3).set_Name("optMag3");
			((Control)optMag3).set_Size(new Size(155, 17));
			((Control)optMag3).set_TabIndex(8);
			((Control)optMag3).set_Text("Magnitude brighter than 3.0");
			((ButtonBase)optMag3).set_UseVisualStyleBackColor(true);
			optMag3.add_CheckedChanged((EventHandler)optMag3_CheckedChanged);
			((Control)optMag2).set_AutoSize(true);
			((Control)optMag2).set_Location(new Point(130, 15));
			((Control)optMag2).set_Name("optMag2");
			((Control)optMag2).set_Size(new Size(155, 17));
			((Control)optMag2).set_TabIndex(7);
			((Control)optMag2).set_Text("Magnitude brighter than 2.0");
			((ButtonBase)optMag2).set_UseVisualStyleBackColor(true);
			optMag2.add_CheckedChanged((EventHandler)optMag2_CheckedChanged);
			((Control)optAll).set_AutoSize(true);
			((Control)optAll).set_Location(new Point(25, 57));
			((Control)optAll).set_Name("optAll");
			((Control)optAll).set_Size(new Size(44, 17));
			((Control)optAll).set_TabIndex(6);
			((Control)optAll).set_Text("ALL");
			((ButtonBase)optAll).set_UseVisualStyleBackColor(true);
			optAll.add_CheckedChanged((EventHandler)optAll_CheckedChanged);
			((Control)optBayer).set_AutoSize(true);
			((Control)optBayer).set_Location(new Point(25, 36));
			((Control)optBayer).set_Name("optBayer");
			((Control)optBayer).set_Size(new Size(83, 17));
			((Control)optBayer).set_TabIndex(5);
			((Control)optBayer).set_Text("Bayer letters");
			((ButtonBase)optBayer).set_UseVisualStyleBackColor(true);
			optBayer.add_CheckedChanged((EventHandler)optBayer_CheckedChanged);
			((Control)optProper).set_AutoSize(true);
			((Control)optProper).set_Location(new Point(25, 15));
			((Control)optProper).set_Name("optProper");
			((Control)optProper).set_Size(new Size(90, 17));
			((Control)optProper).set_TabIndex(4);
			((Control)optProper).set_Text("Proper names");
			((ButtonBase)optProper).set_UseVisualStyleBackColor(true);
			optProper.add_CheckedChanged((EventHandler)optProper_CheckedChanged);
			cmbNames.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbNames).set_FormattingEnabled(true);
			((Control)cmbNames).set_Location(new Point(44, 79));
			cmbNames.set_MaxDropDownItems(20);
			((Control)cmbNames).set_Name("cmbNames");
			((Control)cmbNames).set_Size(new Size(220, 21));
			((Control)cmbNames).set_TabIndex(0);
			cmbNames.add_SelectedIndexChanged((EventHandler)cmbNames_SelectedIndexChanged);
			((Control)grpPlanets).set_Anchor((AnchorStyles)1);
			((Control)grpPlanets).get_Controls().Add((Control)(object)cmdFindPlanet);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optPluto);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optNeptune);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optUranus);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optSaturn);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optJupiter);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optMars);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optVenus);
			((Control)grpPlanets).get_Controls().Add((Control)(object)optMercury);
			((Control)grpPlanets).set_Location(new Point(60, 190));
			((Control)grpPlanets).set_Name("grpPlanets");
			((Control)grpPlanets).set_Size(new Size(582, 79));
			((Control)grpPlanets).set_TabIndex(2);
			grpPlanets.set_TabStop(false);
			((Control)grpPlanets).set_Text("Planets");
			((Control)cmdFindPlanet).set_Location(new Point(485, 20));
			((Control)cmdFindPlanet).set_Name("cmdFindPlanet");
			((Control)cmdFindPlanet).set_Size(new Size(91, 46));
			((Control)cmdFindPlanet).set_TabIndex(8);
			((Control)cmdFindPlanet).set_Text("Find Planet");
			((ButtonBase)cmdFindPlanet).set_UseVisualStyleBackColor(true);
			((Control)cmdFindPlanet).add_Click((EventHandler)cmdFindPlanet_Click);
			((Control)optPluto).set_AutoSize(true);
			((Control)optPluto).set_Location(new Point(380, 44));
			((Control)optPluto).set_Name("optPluto");
			((Control)optPluto).set_Size(new Size(49, 17));
			((Control)optPluto).set_TabIndex(7);
			((Control)optPluto).set_Text("Pluto");
			((ButtonBase)optPluto).set_UseVisualStyleBackColor(true);
			((Control)optNeptune).set_AutoSize(true);
			((Control)optNeptune).set_Location(new Point(380, 21));
			((Control)optNeptune).set_Name("optNeptune");
			((Control)optNeptune).set_Size(new Size(66, 17));
			((Control)optNeptune).set_TabIndex(6);
			((Control)optNeptune).set_Text("Neptune");
			((ButtonBase)optNeptune).set_UseVisualStyleBackColor(true);
			((Control)optUranus).set_AutoSize(true);
			((Control)optUranus).set_Location(new Point(258, 44));
			((Control)optUranus).set_Name("optUranus");
			((Control)optUranus).set_Size(new Size(59, 17));
			((Control)optUranus).set_TabIndex(5);
			((Control)optUranus).set_Text("Uranus");
			((ButtonBase)optUranus).set_UseVisualStyleBackColor(true);
			((Control)optSaturn).set_AutoSize(true);
			((Control)optSaturn).set_Location(new Point(258, 21));
			((Control)optSaturn).set_Name("optSaturn");
			((Control)optSaturn).set_Size(new Size(56, 17));
			((Control)optSaturn).set_TabIndex(4);
			((Control)optSaturn).set_Text("Saturn");
			((ButtonBase)optSaturn).set_UseVisualStyleBackColor(true);
			((Control)optJupiter).set_AutoSize(true);
			optJupiter.set_Checked(true);
			((Control)optJupiter).set_Location(new Point(139, 44));
			((Control)optJupiter).set_Name("optJupiter");
			((Control)optJupiter).set_Size(new Size(56, 17));
			((Control)optJupiter).set_TabIndex(3);
			optJupiter.set_TabStop(true);
			((Control)optJupiter).set_Text("Jupiter");
			((ButtonBase)optJupiter).set_UseVisualStyleBackColor(true);
			((Control)optMars).set_AutoSize(true);
			((Control)optMars).set_Location(new Point(139, 21));
			((Control)optMars).set_Name("optMars");
			((Control)optMars).set_Size(new Size(48, 17));
			((Control)optMars).set_TabIndex(2);
			((Control)optMars).set_Text("Mars");
			((ButtonBase)optMars).set_UseVisualStyleBackColor(true);
			((Control)optVenus).set_AutoSize(true);
			((Control)optVenus).set_Location(new Point(21, 44));
			((Control)optVenus).set_Name("optVenus");
			((Control)optVenus).set_Size(new Size(55, 17));
			((Control)optVenus).set_TabIndex(1);
			((Control)optVenus).set_Text("Venus");
			((ButtonBase)optVenus).set_UseVisualStyleBackColor(true);
			((Control)optMercury).set_AutoSize(true);
			((Control)optMercury).set_Location(new Point(21, 21));
			((Control)optMercury).set_Name("optMercury");
			((Control)optMercury).set_Size(new Size(63, 17));
			((Control)optMercury).set_TabIndex(0);
			((Control)optMercury).set_Text("Mercury");
			((ButtonBase)optMercury).set_UseVisualStyleBackColor(true);
			((Control)grpAsteroids).set_Anchor((AnchorStyles)1);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)label16);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmdFindAsteroid);
			((Control)grpAsteroids).get_Controls().Add((Control)(object)cmbAsteroid);
			((Control)grpAsteroids).set_Location(new Point(60, 278));
			((Control)grpAsteroids).set_Name("grpAsteroids");
			((Control)grpAsteroids).set_Size(new Size(582, 60));
			((Control)grpAsteroids).set_TabIndex(3);
			grpAsteroids.set_TabStop(false);
			((Control)grpAsteroids).set_Text("Asteroids");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(117, 27));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(45, 13));
			((Control)label16).set_TabIndex(2);
			((Control)label16).set_Text("Asteroid");
			((Control)cmdFindAsteroid).set_Location(new Point(485, 10));
			((Control)cmdFindAsteroid).set_Name("cmdFindAsteroid");
			((Control)cmdFindAsteroid).set_Size(new Size(91, 46));
			((Control)cmdFindAsteroid).set_TabIndex(1);
			((Control)cmdFindAsteroid).set_Text("Find Asteroid");
			((ButtonBase)cmdFindAsteroid).set_UseVisualStyleBackColor(true);
			((Control)cmdFindAsteroid).add_Click((EventHandler)cmdFindAsteroid_Click);
			cmbAsteroid.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAsteroid).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAsteroid).set_FormattingEnabled(true);
			((Control)cmbAsteroid).set_Location(new Point(179, 24));
			cmbAsteroid.set_MaxDropDownItems(20);
			((Control)cmbAsteroid).set_Name("cmbAsteroid");
			((Control)cmbAsteroid).set_Size(new Size(138, 22));
			((Control)cmbAsteroid).set_TabIndex(0);
			((Control)grpCoordinates).set_Anchor((AnchorStyles)1);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label18);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label11);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)txtLabel);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)cmbClusters);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label10);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)updnMag);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label15);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label7);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)txtDS1);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)txtDM1);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)txtDD1);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label8);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label9);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label13);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label4);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label5);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)label6);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)txtRS1);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)txtRM1);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)txtRH1);
			((Control)grpCoordinates).get_Controls().Add((Control)(object)cmdFindObject);
			((Control)grpCoordinates).set_Location(new Point(60, 347));
			((Control)grpCoordinates).set_Name("grpCoordinates");
			((Control)grpCoordinates).set_Size(new Size(582, 92));
			((Control)grpCoordinates).set_TabIndex(4);
			grpCoordinates.set_TabStop(false);
			((Control)grpCoordinates).set_Text("J2000  Coordinates   -  OR  -  Select open cluster");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(49, 68));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(113, 13));
			((Control)label18).set_TabIndex(2);
			((Control)label18).set_Text("Select an open cluster");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(356, 32));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(35, 13));
			((Control)label11).set_TabIndex(50);
			((Control)label11).set_Text("Name");
			((Control)txtLabel).set_Location(new Point(391, 29));
			((TextBoxBase)txtLabel).set_MaxLength(9);
			((Control)txtLabel).set_Name("txtLabel");
			((Control)txtLabel).set_Size(new Size(74, 20));
			((Control)txtLabel).set_TabIndex(49);
			cmbClusters.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbClusters).set_FormattingEnabled(true);
			cmbClusters.get_Items().AddRange(new object[1] { "[None]" });
			((Control)cmbClusters).set_Location(new Point(179, 65));
			cmbClusters.set_MaxDropDownItems(20);
			((Control)cmbClusters).set_Name("cmbClusters");
			((Control)cmbClusters).set_Size(new Size(138, 21));
			((Control)cmbClusters).set_TabIndex(0);
			cmbClusters.add_SelectedIndexChanged((EventHandler)cmbClusters_SelectedIndexChanged);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(267, 32));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(28, 13));
			((Control)label10).set_TabIndex(48);
			((Control)label10).set_Text("Mag");
			updnMag.set_DecimalPlaces(1);
			updnMag.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMag).set_Location(new Point(297, 29));
			updnMag.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			((Control)updnMag).set_Name("updnMag");
			((Control)updnMag).set_Size(new Size(43, 20));
			((Control)updnMag).set_TabIndex(47);
			updnMag.set_Value(new decimal(new int[4] { 6, 0, 0, 0 }));
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(135, 32));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(27, 13));
			((Control)label15).set_TabIndex(46);
			((Control)label15).set_Text("Dec");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(176, 16));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(13, 13));
			((Control)label7).set_TabIndex(43);
			((Control)label7).set_Text("o");
			((Control)txtDS1).set_Location(new Point(231, 29));
			((Control)txtDS1).set_Name("txtDS1");
			((Control)txtDS1).set_Size(new Size(24, 20));
			((Control)txtDS1).set_TabIndex(42);
			((Control)txtDS1).set_Text("0");
			((Control)txtDM1).set_Location(new Point(199, 29));
			((Control)txtDM1).set_Name("txtDM1");
			((Control)txtDM1).set_Size(new Size(23, 20));
			((Control)txtDM1).set_TabIndex(41);
			((Control)txtDM1).set_Text("0");
			((Control)txtDD1).set_Location(new Point(164, 29));
			((Control)txtDD1).set_Name("txtDD1");
			((Control)txtDD1).set_Size(new Size(28, 20));
			((Control)txtDD1).set_TabIndex(40);
			((Control)txtDD1).set_Text("0");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(210, 21));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(9, 13));
			((Control)label8).set_TabIndex(44);
			((Control)label8).set_Text("'");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(240, 21));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(12, 13));
			((Control)label9).set_TabIndex(45);
			((Control)label9).set_Text("\"");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(4, 32));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(22, 13));
			((Control)label13).set_TabIndex(39);
			((Control)label13).set_Text("RA");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(98, 14));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(12, 13));
			((Control)label4).set_TabIndex(38);
			((Control)label4).set_Text("s");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(64, 14));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(15, 13));
			((Control)label5).set_TabIndex(37);
			((Control)label5).set_Text("m");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(36, 14));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(13, 13));
			((Control)label6).set_TabIndex(36);
			((Control)label6).set_Text("h");
			((Control)txtRS1).set_Location(new Point(91, 29));
			((Control)txtRS1).set_Name("txtRS1");
			((Control)txtRS1).set_Size(new Size(30, 20));
			((Control)txtRS1).set_TabIndex(35);
			((Control)txtRS1).set_Text("0");
			((Control)txtRM1).set_Location(new Point(59, 29));
			((Control)txtRM1).set_Name("txtRM1");
			((Control)txtRM1).set_Size(new Size(23, 20));
			((Control)txtRM1).set_TabIndex(34);
			((Control)txtRM1).set_Text("0");
			((Control)txtRH1).set_Location(new Point(29, 29));
			((Control)txtRH1).set_Name("txtRH1");
			((Control)txtRH1).set_Size(new Size(23, 20));
			((Control)txtRH1).set_TabIndex(33);
			((Control)txtRH1).set_Text("0");
			((Control)cmdFindObject).set_Location(new Point(485, 11));
			((Control)cmdFindObject).set_Name("cmdFindObject");
			((Control)cmdFindObject).set_Size(new Size(91, 46));
			((Control)cmdFindObject).set_TabIndex(0);
			((Control)cmdFindObject).set_Text("Find Object");
			((ButtonBase)cmdFindObject).set_UseVisualStyleBackColor(true);
			((Control)cmdFindObject).add_Click((EventHandler)cmdFindObject_Click);
			((Control)updnStartYear).set_Anchor((AnchorStyles)1);
			((Control)updnStartYear).set_Location(new Point(256, 32));
			updnStartYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(65, 20));
			((Control)updnStartYear).set_TabIndex(5);
			updnStartYear.set_Value(new decimal(new int[4] { 2015, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)updnStartYear).add_MouseClick(new MouseEventHandler(updnStartYear_MouseClick));
			((Control)updnEndYear).set_Anchor((AnchorStyles)1);
			((Control)updnEndYear).set_Location(new Point(378, 32));
			updnEndYear.set_Maximum(new decimal(new int[4] { 9999, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 9999, 0, 0, -2147483648 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(65, 20));
			((Control)updnEndYear).set_TabIndex(6);
			updnEndYear.set_Value(new decimal(new int[4] { 2015, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnEndYear).add_MouseClick(new MouseEventHandler(updnEndYear_MouseClick));
			((Control)label12).set_Anchor((AnchorStyles)1);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 14.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(125, 30));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(123, 24));
			((Control)label12).set_TabIndex(7);
			((Control)label12).set_Text("Search from");
			((Control)label14).set_Anchor((AnchorStyles)1);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 14.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(339, 30));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(33, 24));
			((Control)label14).set_TabIndex(8);
			((Control)label14).set_Text("to ");
			((Control)pbarFind).set_Anchor((AnchorStyles)1);
			((Control)pbarFind).set_Location(new Point(485, 41));
			((Control)pbarFind).set_Name("pbarFind");
			((Control)pbarFind).set_Size(new Size(88, 11));
			((Control)pbarFind).set_TabIndex(9);
			((Control)pbarFind).set_Visible(false);
			((Control)cmdPlotAll).set_Anchor((AnchorStyles)1);
			((Control)cmdPlotAll).set_Location(new Point(324, 456));
			((Control)cmdPlotAll).set_Name("cmdPlotAll");
			((Control)cmdPlotAll).set_Size(new Size(151, 29));
			((Control)cmdPlotAll).set_TabIndex(10);
			((Control)cmdPlotAll).set_Text("Plot all listed events");
			((ButtonBase)cmdPlotAll).set_UseVisualStyleBackColor(true);
			((Control)cmdPlotAll).set_Visible(false);
			((Control)cmdPlotAll).add_Click((EventHandler)cmdPlotAll_Click);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(10, 478));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(234, 13));
			((Control)label17).set_TabIndex(12);
			((Control)label17).set_Text("Right-click on an item for display options");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withListOfEventsToolStripMenuItem,
				(ToolStripItem)hToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(699, 24));
			((Control)menuStrip1).set_TabIndex(13);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListOfEventsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListOfEventsToolStripMenuItem).set_Name("withListOfEventsToolStripMenuItem");
			((ToolStripItem)withListOfEventsToolStripMenuItem).set_Size(new Size(138, 20));
			((ToolStripItem)withListOfEventsToolStripMenuItem).set_Text("with List of Events ...    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("&Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(144, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("&Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)hToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)hToolStripMenuItem).set_Name("hToolStripMenuItem");
			((ToolStripItem)hToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)hToolStripMenuItem).set_Text("&Help   ");
			((ToolStripItem)hToolStripMenuItem).add_Click((EventHandler)hToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(323, 456));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(151, 29));
			((Control)cmdCancel).set_TabIndex(14);
			((Control)cmdCancel).set_Text("Cancel plot");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)optColour).set_AutoSize(true);
			optColour.set_Checked(true);
			((Control)optColour).set_Location(new Point(481, 462));
			((Control)optColour).set_Name("optColour");
			((Control)optColour).set_Size(new Size(55, 17));
			((Control)optColour).set_TabIndex(15);
			optColour.set_TabStop(true);
			((Control)optColour).set_Text("Colour");
			((ButtonBase)optColour).set_UseVisualStyleBackColor(true);
			((Control)optBW).set_AutoSize(true);
			((Control)optBW).set_Location(new Point(542, 462));
			((Control)optBW).set_Name("optBW");
			((Control)optBW).set_Size(new Size(49, 17));
			((Control)optBW).set_TabIndex(16);
			((Control)optBW).set_Text("B&&W");
			((ButtonBase)optBW).set_UseVisualStyleBackColor(true);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(699, 711));
			((Control)this).get_Controls().Add((Control)(object)optBW);
			((Control)this).get_Controls().Add((Control)(object)optColour);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)pbarFind);
			((Control)this).get_Controls().Add((Control)(object)label14);
			((Control)this).get_Controls().Add((Control)(object)label12);
			((Control)this).get_Controls().Add((Control)(object)updnEndYear);
			((Control)this).get_Controls().Add((Control)(object)updnStartYear);
			((Control)this).get_Controls().Add((Control)(object)grpCoordinates);
			((Control)this).get_Controls().Add((Control)(object)grpAsteroids);
			((Control)this).get_Controls().Add((Control)(object)grpPlanets);
			((Control)this).get_Controls().Add((Control)(object)grpStars);
			((Control)this).get_Controls().Add((Control)(object)lstEvents);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)cmdPlotAll);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarFindAnOccultation", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationLunarFindAnOccultation);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(600, 500));
			((Control)this).set_Name("Lunar_FindAnOccultation");
			((Control)this).set_Text("Find occultations of a particular object");
			((Form)this).add_Load((EventHandler)Lunar_FindAnOccultation_Load);
			((Control)this).add_Resize((EventHandler)Lunar_FindAnOccultation_Resize);
			((Control)contextMenuStrip1).ResumeLayout(false);
			((Control)grpStars).ResumeLayout(false);
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)grpPlanets).ResumeLayout(false);
			((Control)grpPlanets).PerformLayout();
			((Control)grpAsteroids).ResumeLayout(false);
			((Control)grpAsteroids).PerformLayout();
			((Control)grpCoordinates).ResumeLayout(false);
			((Control)grpCoordinates).PerformLayout();
			((ISupportInitialize)updnMag).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
