using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using GaiaDoubles;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class StarCatalogueDetails : Form
	{
		private readonly string AppPath;

		private const double Radian = 180.0 / Math.PI;

		private bool NeedToClearOrbitGraphic;

		private bool UCAC4Present;

		private double RAforDouble;

		private double DecForDouble;

		private string RAstar = "";

		private string DECstar = "";

		private IContainer components;

		private GroupBox groupBox1;

		internal TextBox txtStar;

		private Button cmdGetStar;

		internal RadioButton optSAO;

		internal RadioButton optZC;

		internal RadioButton optTyc;

		internal RadioButton optHip;

		internal RadioButton optXZ;

		private Label labelRA;

		private Label labelDec;

		private Label labelMag;

		private Label labelID;

		private Label labelVariable;

		private ListBox lstDoubles;

		private ListBox lstDiscoverers;

		private Label label1;

		private Label labelDouble;

		private Label label2;

		private GroupBox grpDoubles;

		private GroupBox grpVariables;

		private GroupBox grpOrbit;

		private Label label3;

		private ListBox lstOrbits;

		private PictureBox picOrbit;

		private Label label4;

		private Button cmdToday;

		private Label label5;

		internal NumericUpDown updnYear;

		private Button cmdReDraw;

		private TextBox txtT2;

		private TextBox txtTzone;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withDetailsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ListBox lstEphem1;

		private Label label6;

		private ListBox lstEphem3;

		private ListBox lstEphem2;

		private Label label7;

		private Label label9;

		private Label label8;

		private Button cmdShowWDS_IF;

		private Label label10;

		internal RadioButton optNOMAD;

		internal RadioButton optB1;

		private Button cmdDisplayOrbits;

		private RadioButton optPPMXL;

		private RadioButton optUCAC4;

		private Label lblKepler2;

		private Label lblPASepn;

		internal RadioButton optGaia;

		private TextBox txtT3;

		private Label label11;

		private Label label14;

		private Label label13;

		private Label label12;

		private Label lblJ;

		private Button cmdGaiaDoubles;

		private Label label15;

		public StarCatalogueDetails()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void StarCatalogueDetails_Load(object sender, EventArgs e)
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
			string text = Settings.Default.NOMAD_Path;
			if (!text.ToLower().Contains("nomad") | !File.Exists(text + "\\090\\m0900.cat"))
			{
				text = Settings.Default.NOMAD_Short_path;
			}
			RadioButton obj = optB1;
			bool enabled;
			((Control)optNOMAD).set_Enabled(enabled = text.ToLower().Contains("nomad"));
			((Control)obj).set_Enabled(enabled);
			UCAC4Present = File.Exists(Settings.Default.UCAC4_Path + "\\u4b\\z001");
			((Control)optGaia).set_Enabled(File.Exists(AppPath + "\\Resource Files\\Gaia\\Gaia14_DR2.bin") | File.Exists(AppPath + "\\Resource Files\\Gaia\\Gaia11_DR2.bin") | File.Exists(AppPath + "\\Resource Files\\Gaia\\Gaia14.bin"));
		}

		private void StarCatalogueDetails_FormClosed(object sender, FormClosedEventArgs e)
		{
		}

		private void cmdGetStar_Click(object sender, EventArgs e)
		{
			GetStar();
		}

		private void GetStar()
		{
			int result = 0;
			bool flag = int.TryParse(((Control)txtStar).get_Text(), out result);
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			Interferometric_Plus_WDS.Orbits.Clear();
			if (optZC.get_Checked() | optSAO.get_Checked() | optXZ.get_Checked())
			{
				if (optZC.get_Checked())
				{
					flag &= XZ80Q.Get_ZC_Star(result);
				}
				else if (optSAO.get_Checked())
				{
					flag &= XZ80Q.Get_SAO_Star(result);
				}
				else if (optXZ.get_Checked())
				{
					flag &= XZ80Q.Get_XZ_Star(result);
				}
				if (flag)
				{
					DisplayXZstars();
				}
			}
			else if (optHip.get_Checked())
			{
				DisplayHipStar(result);
			}
			else if (optTyc.get_Checked())
			{
				DisplayTycho2Star();
			}
			else if (optUCAC4.get_Checked())
			{
				DisplayUCAC4Star();
			}
			else if (optGaia.get_Checked())
			{
				DisplayGaiaStar();
			}
			else if (optB1.get_Checked())
			{
				DisplayB1Star();
			}
			else if (optNOMAD.get_Checked())
			{
				DisplayNOMADStar();
			}
			else if (optPPMXL.get_Checked())
			{
				DisplayPPMXLStar();
			}
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		internal void ExternalDisplayXZStar(string StarID)
		{
			//IL_008f: Unknown result type (might be due to invalid IL or missing references)
			int result;
			try
			{
				if (StarID.Substring(0, 1) == "X")
				{
					int.TryParse(StarID.Substring(1, 6), out result);
					XZ80Q.Get_XZ_Star(result);
					optXZ.set_Checked(true);
				}
				else
				{
					int.TryParse(StarID.Substring(1, 6), out result);
					if (result < 9000)
					{
						XZ80Q.Get_ZC_Star(result);
						optZC.set_Checked(true);
					}
					else
					{
						XZ80Q.Get_SAO_Star(result);
						optSAO.set_Checked(true);
					}
				}
			}
			catch
			{
				MessageBox.Show("Cannot display star details for " + StarID, "Not a star", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			((Control)txtStar).set_Text(result.ToString());
			DisplayXZstars();
		}

		internal void DisplayXZstars()
		{
			string Basis = "";
			Interferometric_Plus_WDS.Orbits.Clear();
			lstEphem1.get_Items().Clear();
			lstEphem2.get_Items().Clear();
			lstEphem3.get_Items().Clear();
			ArrayList arrayList = new ArrayList();
			string text = " XZ " + XZ80Q.XZ;
			if (XZ80Q.SAO > 0)
			{
				text = text + " = SAO " + XZ80Q.SAO;
			}
			if (XZ80Q.ZC > 0)
			{
				text = text + " = ZC " + XZ80Q.ZC;
				if (LunarOccultations.ZCName(XZ80Q.ZC, out var Name))
				{
					int num = Name.IndexOf("=");
					text = text + " = " + Name.Substring(num + 1).Trim();
				}
			}
			int NumMeasures;
			bool InvalidDiameter;
			double num2 = Utilities.StarDiameter_CHARM2_CADARS(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, XZ80Q.Dec_rad * (180.0 / Math.PI), XZ80Q.Mv, XZ80Q.Mp, XZ80Q.Mr, out Basis, out NumMeasures, out InvalidDiameter);
			RAstar = Utilities.DEGtoDMS(XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
			DECstar = Utilities.DEGtoDMS(XZ80Q.Dec_rad * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
			((Control)labelID).set_Text(text);
			((Control)labelRA).set_Text(" RA =  " + RAstar + "   PM = " + string.Format("{0,5:F5}s", XZ80Q.PMRA_rad * 3600.0 / 15.0 * (180.0 / Math.PI)));
			((Control)labelDec).set_Text("Dec = " + DECstar + "    PM = " + string.Format("{0,4:F4}\"", XZ80Q.PMDec_rad * 3600.0 * (180.0 / Math.PI)));
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append($" Mv = {XZ80Q.Mv:F2}" + $", Mb = {XZ80Q.Mp:F2}" + $", Mr = {XZ80Q.Mr:F2}    Spectrum " + XZ80Q.Spectrum.Replace(" ", "_"));
			if (NumMeasures > 0)
			{
				stringBuilder.Append($"    Dia: {num2:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures));
			}
			((Control)labelMag).set_Text(stringBuilder.ToString());
			DisplayKepler2ID(XZ80Q.RA_rad, XZ80Q.Dec_rad);
			RAforDouble = XZ80Q.RA_rad * (180.0 / Math.PI) / 15.0;
			DecForDouble = XZ80Q.Dec_rad * (180.0 / Math.PI);
			if (XZ80Q.VariableFlag != " ")
			{
				((Control)grpVariables).set_Enabled(true);
				LunarOccultations.VariableStarInfo(XZ80Q.XZ, Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day) + (double)DateTime.Now.Hour / 24.0, out var VariableDetails);
				((Control)labelVariable).set_Text(VariableDetails);
			}
			else
			{
				((Control)labelVariable).set_Text("");
				((Control)grpVariables).set_Enabled(false);
			}
			if (XZ80Q.DoubleFlag != " ")
			{
				((Control)grpDoubles).set_Enabled(true);
				DoubleStars.GetXZDoubleList(XZ80Q.XZ);
				lstDoubles.get_Items().Clear();
				lstDiscoverers.get_Items().Clear();
				lstOrbits.get_Items().Clear();
				if (DoubleStars.XZDoubleList.Count > 0)
				{
					((Control)labelDouble).set_Text(DoubleStars.XZDoubleList[0]!.ToString());
				}
				else if (XZ80Q.DoubleFlag.ToUpper() != "K")
				{
					((Control)labelDouble).set_Text("***  Error in XZDoubles.dat File. Please report the star XZ number.  ***");
				}
				for (int i = 1; i < DoubleStars.XZDoubleList.Count; i++)
				{
					lstDoubles.get_Items().Add((object)DoubleStars.XZDoubleList[i]!.ToString());
					arrayList.Add(DoubleStars.XZDoubleList[i]!.ToString()!.Substring(0, 3));
					if (!(DoubleStars.XZDoubleList[i]!.ToString()!.Substring(0, 3).ToUpper() == "OCC"))
					{
						continue;
					}
					int num3 = int.Parse(DoubleStars.XZDoubleList[i]!.ToString()!.Substring(3, 4));
					using StreamReader streamReader = new StreamReader(AppPath + "\\Resource Files\\xzdoubles discoveries.dat");
					if (num3 >= streamReader.BaseStream.Length / 151)
					{
						continue;
					}
					string text2;
					try
					{
						streamReader.BaseStream.Seek(151 * (num3 - 1), SeekOrigin.Begin);
						text2 = streamReader.ReadLine();
					}
					catch
					{
						text2 = "".PadRight(149);
					}
					string text3 = DoubleStars.XZDoubleList[i]!.ToString()!.Substring(0, 7);
					if (!int.TryParse(text2.Substring(29, 2), out var result))
					{
						result = 0;
					}
					lstDoubles.get_Items().Add((object)(text3 + " observed by " + text2.Substring(34, 24).Trim() + " on " + text2.Substring(25, 4) + " " + Utilities.ShortMonths[result] + " " + text2.Substring(31, 2) + ". Refer: " + text2.Substring(59, 24).Trim()));
					using StreamReader streamReader2 = new StreamReader(AppPath + "\\Resource Files\\xzConfirmations.dat");
					do
					{
						string text4 = streamReader2.ReadLine();
						if (text4.Substring(0, 7) == text3)
						{
							if (!int.TryParse(text4.Substring(18, 2), out result))
							{
								result = 0;
							}
							lstDoubles.get_Items().Add((object)("        ........ by " + text4.Substring(24, 24).Trim() + " on " + text4.Substring(14, 4) + " " + Utilities.ShortMonths[result] + " " + text4.Substring(20, 2)));
						}
					}
					while (int.Parse(DoubleStars.XZDoubleList[i]!.ToString()!.Substring(3, 4)) >= num3 && !streamReader2.EndOfStream);
				}
				arrayList.Sort();
				for (int j = 0; j < arrayList.Count; j++)
				{
					if (j == 0)
					{
						lstDiscoverers.get_Items().Add((object)DoubleStars.WDSName(arrayList[0]!.ToString()));
					}
					else if (arrayList[j]!.ToString() != arrayList[j - 1]!.ToString())
					{
						lstDiscoverers.get_Items().Add((object)DoubleStars.WDSName(arrayList[j]!.ToString()));
					}
				}
				if (DoubleStars.XZOrbitsList.Count > 0)
				{
					((Control)grpOrbit).set_Enabled(true);
					for (int k = 0; k < DoubleStars.XZOrbitsList.Count; k++)
					{
						lstOrbits.get_Items().Add((object)DoubleStars.XZOrbitsList[k].OrbitElements());
					}
					Interferometric_Plus_WDS.AddOrbitImageReference(DoubleStars.XZOrbitsList[0].WDS_ID);
					((ListControl)lstOrbits).set_SelectedIndex(0);
					FiveYearEphemeris();
					((Control)cmdDisplayOrbits).set_Enabled(Interferometric_Plus_WDS.Orbits.Count > 0);
				}
				else
				{
					lstOrbits.get_Items().Clear();
					((Control)grpOrbit).set_Enabled(false);
					if (NeedToClearOrbitGraphic)
					{
						ClearOrbitPlot();
					}
					((Control)cmdDisplayOrbits).set_Enabled(false);
				}
			}
			else
			{
				lstDoubles.get_Items().Clear();
				lstDiscoverers.get_Items().Clear();
				lstOrbits.get_Items().Clear();
				((Control)labelDouble).set_Text("");
				((Control)grpDoubles).set_Enabled(false);
				((Control)grpOrbit).set_Enabled(false);
				if (NeedToClearOrbitGraphic)
				{
					ClearOrbitPlot();
				}
			}
		}

		private void DisplayKepler2ID(double RA, double Dec)
		{
			bool flag = false;
			if ((Kepler2.NumKep2Stars < 1) & Kepler2.Kepler2DataExists)
			{
				Kepler2.Initialise_Kepler2_ForAsteroids();
			}
			flag = Kepler2.StarInKepler2(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), out var RecNum);
			((Control)lblKepler2).set_Visible(flag);
			((Control)lblKepler2).set_Text("Kepler2 target, EPIC_ID = " + Kepler2.K2[RecNum].EPIC_ID);
		}

		private void lstOrbits_SelectedIndexChanged(object sender, EventArgs e)
		{
			DrawOrbit();
		}

		private void cmdToday_Click(object sender, EventArgs e)
		{
			InsertToday();
		}

		internal void InsertToday()
		{
			updnYear.set_Value((decimal)((double)DateTime.Now.Year + (double)(DateTime.Now.Month - 1) / 12.0 + (double)DateTime.Now.Day / 365.0));
			if (lstOrbits.get_Items().get_Count() > 0)
			{
				DrawOrbit();
			}
		}

		private void cmdReDraw_Click(object sender, EventArgs e)
		{
			DrawOrbit();
			FiveYearEphemeris();
		}

		private void ClearOrbitPlot()
		{
			int width = ((Control)picOrbit).get_Width();
			int height = ((Control)picOrbit).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Control.get_DefaultBackColor());
			picOrbit.set_Image((Image)image);
			graphics.Dispose();
			NeedToClearOrbitGraphic = false;
			lstEphem1.get_Items().Clear();
			lstEphem2.get_Items().Clear();
			lstEphem3.get_Items().Clear();
			((Control)lblPASepn).set_Text("PA =  x.x°   Sep = s.s\"");
		}

		private void DrawOrbit()
		{
			int width = ((Control)picOrbit).get_Width();
			int height = ((Control)picOrbit).get_Height();
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			graphics.Clear(Control.get_DefaultBackColor());
			Pen pen = new Pen(Color.Black, 1f);
			Font font = new Font("Courier New", 8f);
			float num = (float)width / 2f;
			float num2 = num;
			int selectedIndex = ((ListControl)lstOrbits).get_SelectedIndex();
			float num3 = (float)((double)num / DoubleStars.XZOrbitsList[selectedIndex].SemiMajor / (1.0 + DoubleStars.XZOrbitsList[selectedIndex].Eccentricity) / 1.2);
			float num4 = 0f;
			float num5 = 0f;
			float x = 0f;
			float y = 0f;
			graphics.FillEllipse(Brushes.DarkBlue, num - 5f, num2 - 5f, 10f, 10f);
			double PA;
			double Sep;
			for (double num6 = 0.0; num6 <= 1.001; num6 += 0.02)
			{
				DoubleStars.XZOrbitsList[selectedIndex].PAandSep_byOrbitFraction(num6, out PA, out Sep);
				num4 = (float)((double)num - (double)num3 * Sep * Math.Sin(PA / (180.0 / Math.PI)));
				num5 = (float)((double)num2 - (double)num3 * Sep * Math.Cos(PA / (180.0 / Math.PI)));
				if (num6 > 0.0)
				{
					graphics.DrawLine(pen, x, y, num4, num5);
				}
				x = num4;
				y = num5;
			}
			DoubleStars.XZOrbitsList[selectedIndex].PAandSep((double)updnYear.get_Value(), out PA, out Sep);
			num4 = (float)((double)num - (double)num3 * Sep * Math.Sin(PA / (180.0 / Math.PI)));
			num5 = (float)((double)num2 - (double)num3 * Sep * Math.Cos(PA / (180.0 / Math.PI)));
			graphics.FillEllipse(Brushes.DarkBlue, num4 - 3f, num5 - 3f, 6f, 6f);
			((Control)lblPASepn).set_Text(string.Format("PA =  {0,2:f1}°   Sep = {1,2:f3}\"", PA, Sep));
			float num7 = 0f;
			double semiMajor = DoubleStars.XZOrbitsList[selectedIndex].SemiMajor;
			if (semiMajor < 0.01)
			{
				num7 = 0.01f;
			}
			if (semiMajor > 0.01)
			{
				num7 = 0.03f;
			}
			if (semiMajor > 0.03)
			{
				num7 = 0.1f;
			}
			if (semiMajor > 0.1)
			{
				num7 = 0.3f;
			}
			if (semiMajor > 0.3)
			{
				num7 = 1f;
			}
			if (semiMajor > 1.0)
			{
				num7 = 3f;
			}
			if (semiMajor > 3.0)
			{
				num7 = 10f;
			}
			if (semiMajor > 10.0)
			{
				num7 = 30f;
			}
			string s = num7 + "\"";
			graphics.DrawLine(pen, num - num7 * num3 / 2f, 2f * num2 + 5f, num + num7 * num3 / 2f, 2f * num2 + 5f);
			graphics.DrawLine(pen, num - num7 * num3 / 2f, 2f * num2, num - num7 * num3 / 2f, 2f * num2 + 10f);
			graphics.DrawLine(pen, num + num7 * num3 / 2f, 2f * num2, num + num7 * num3 / 2f, 2f * num2 + 10f);
			graphics.DrawString(s, font, Brushes.Black, num - 10f, 2f * num2 + 5f);
			picOrbit.set_Image((Image)image);
			graphics.Dispose();
			NeedToClearOrbitGraphic = true;
			updnYear.set_Increment(100m);
			if (DoubleStars.XZOrbitsList[selectedIndex].Period < 500.0)
			{
				updnYear.set_Increment(10m);
			}
			if (DoubleStars.XZOrbitsList[selectedIndex].Period < 50.0)
			{
				updnYear.set_Increment(1m);
			}
			if (DoubleStars.XZOrbitsList[selectedIndex].Period < 5.0)
			{
				updnYear.set_Increment(0.1m);
			}
			if (DoubleStars.XZOrbitsList[selectedIndex].Period < 0.5)
			{
				updnYear.set_Increment(0.01m);
			}
		}

		private void FiveYearEphemeris()
		{
			lstEphem1.get_Items().Clear();
			lstEphem2.get_Items().Clear();
			lstEphem3.get_Items().Clear();
			for (int i = 0; i <= lstOrbits.get_Items().get_Count() - 1; i++)
			{
				double num = Math.Floor((double)updnYear.get_Value()) - 2.0;
				for (int j = 0; j < 5; j++)
				{
					double num2 = num + (double)j;
					DoubleStars.XZOrbitsList[i].PAandSep(num2, out var PA, out var Sep);
					switch (i)
					{
					case 0:
						lstEphem1.get_Items().Add((object)string.Format("{0,4:f0} {1,6:f2} {2,5:f2}", num2, PA, Sep));
						break;
					case 1:
						lstEphem2.get_Items().Add((object)string.Format("{0,4:f0} {1,6:f2} {2,5:f2}", num2, PA, Sep));
						break;
					default:
						lstEphem3.get_Items().Add((object)string.Format("{0,4:f0} {1,6:f2} {2,5:f2}", num2, PA, Sep));
						break;
					}
				}
			}
		}

		private void DisplayHipStar(int HipparcosNumber)
		{
			bool UsedGaia = false;
			int NumMeasures = 0;
			string text = "";
			string SourceFile = "None";
			if (GetStarPosition.GetHipparcosPosition(HipparcosNumber, out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR, out var Parallax_asec, out var Epoch, out UsedGaia, out var _, out var _, out SourceFile, out var _, out var _, out var _, out var _, out var _, out var StarDiameter_mas, out var _, out var _, out var _, out var _, out var CorrectStarIdentifier))
			{
				RAstar = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
				DECstar = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				string text2 = "";
				if (UsedGaia)
				{
					text2 = "    From " + SourceFile;
				}
				string Basis;
				bool InvalidDiameter;
				double num = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
				if (num > 1.0)
				{
					StarDiameter_mas = num;
				}
				else
				{
					StarDiameter_mas /= 1000.0;
					text = "Gaia DR3";
					NumMeasures = 0;
				}
				string text3 = Utilities.StarNameFromHip(HipparcosNumber, MagV);
				if (text3.Length > 0)
				{
					text3 = "  = " + text3;
				}
				string text4 = Utilities.StarIdentifier_ToMag6(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI), MagV, WithConstellation: true);
				if (text4.Length > 0)
				{
					text4 = "  = " + text4;
				}
				((Control)labelID).set_Text($"Hip {HipparcosNumber:F0}" + text4 + text3);
				((Control)labelRA).set_Text(" RA =  " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false) + "   PM = " + string.Format("{0,5:F5}s", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0) + text2);
				((Control)labelDec).set_Text("Dec = " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false) + "    PM = " + string.Format("{0,4:F4}\"", pmDec * (180.0 / Math.PI) * 3600.0) + "     Epoch = " + string.Format("{0,5:F2}", Epoch));
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append($" MagV = {MagV:F2}" + $", MagB = {MagB:F2}" + $", MagR = {MagR:F2}" + $"     Parallax {Parallax_asec:F4}\"");
				if (StarDiameter_mas > 0.0)
				{
					if (NumMeasures > 0)
					{
						stringBuilder.Append($"    Dia: {StarDiameter_mas:.0000}\"" + " [" + text + string.Format(", {0,1} measures]", NumMeasures));
					}
					else
					{
						stringBuilder.Append($"    Dia: {StarDiameter_mas:.0000}\"" + " [" + text + "]");
					}
				}
				else if (UsedGaia)
				{
					stringBuilder.Append("    Dia < 0.0001\"");
				}
				((Control)labelMag).set_Text(stringBuilder.ToString());
				DisplayKepler2ID(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
				RAforDouble = RA * (180.0 / Math.PI) / 15.0;
				DecForDouble = Dec * (180.0 / Math.PI);
			}
			else
			{
				if (CorrectStarIdentifier.Length > 0)
				{
					((Control)labelID).set_Text($"Hip {HipparcosNumber:F0}" + " is held in the Occult Gaia files as " + CorrectStarIdentifier);
				}
				else
				{
					((Control)labelID).set_Text($"Hip {HipparcosNumber:F0}" + " not found");
				}
				((Control)labelRA).set_Text(" RA =  ?   PM = ?");
				((Control)labelDec).set_Text("Dec = ?    PM = ?");
				((Control)labelMag).set_Text(" Mag = ?, Mp = ?");
				RAforDouble = (DecForDouble = 0.0);
				((Control)lblKepler2).set_Visible(false);
			}
			ClearOrbitPlot();
		}

		private void DisplayTycho2Star()
		{
			double Epoch = 0.0;
			double RadialVelocity = 0.0;
			bool UsedGaia = false;
			string Basis = "";
			string SourceFile = "None";
			if (!int.TryParse(((Control)txtTzone).get_Text(), out var result))
			{
				result = 0;
			}
			if (result < 1 || result > 9537)
			{
				return;
			}
			if (!int.TryParse(((Control)txtT2).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 < 1)
			{
				return;
			}
			if (!int.TryParse(((Control)txtT3).get_Text(), out var result3))
			{
				result3 = 1;
			}
			string text = result + "-" + result2;
			if (!GetStarPosition.HaveCheckedForCatalogues)
			{
				GetStarPosition.CheckStarCatsPresent();
			}
			if (GetStarPosition.GetTycho2Position(result, result2, result3, out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR, out var Parallax_asec, out Epoch, out UsedGaia, out var _, out var _, out SourceFile, out RadialVelocity, out var _, out var _, out var _, out var _, out var StarDiameter_mas, out var _, out var _, out var _, out var _))
			{
				RAstar = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
				DECstar = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				string text2 = "";
				if (UsedGaia)
				{
					text2 = "    From " + SourceFile;
				}
				int NumMeasures;
				bool InvalidDiameter;
				double num = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
				if (num > 1.0)
				{
					StarDiameter_mas = num;
				}
				else
				{
					StarDiameter_mas /= 1000.0;
					Basis = "Gaia DR3";
					NumMeasures = 0;
				}
				((Control)labelID).set_Text("TYC " + text);
				((Control)labelRA).set_Text(" RA =  " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false) + "   PM = " + string.Format("{0,5:F5}s", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0) + text2);
				((Control)labelDec).set_Text("Dec = " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false) + "    PM = " + string.Format("{0,4:F4}\"", pmDec * (180.0 / Math.PI) * 3600.0) + "     Epoch = " + string.Format("{0,5:F2}", Epoch));
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append($" MagV = {MagV:F2}" + $", MagB = {MagB:F2}" + $", MagR = {MagR:F2}");
				if (UsedGaia)
				{
					stringBuilder.Append($"     Parallax {Parallax_asec:F4}\"");
				}
				if (StarDiameter_mas > 0.0)
				{
					if (NumMeasures > 0)
					{
						stringBuilder.Append($"    Dia: {StarDiameter_mas:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures));
					}
					else
					{
						stringBuilder.Append($"    Dia: {StarDiameter_mas:.0000}\"" + " [" + Basis + "]");
					}
				}
				else if (UsedGaia)
				{
					stringBuilder.Append("    Dia < 0.0001\"");
				}
				((Control)labelMag).set_Text(stringBuilder.ToString());
				DisplayKepler2ID(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
				RAforDouble = RA * (180.0 / Math.PI) / 15.0;
				DecForDouble = Dec * (180.0 / Math.PI);
			}
			else
			{
				((Control)labelID).set_Text("TYC " + text + " not found");
				((Control)labelRA).set_Text(" RA =  ?   PM = ?");
				((Control)labelDec).set_Text("Dec = ?   PM = ?");
				((Control)labelMag).set_Text(" Mv = ?,  Mr = ?");
				RAforDouble = (DecForDouble = 0.0);
				((Control)lblKepler2).set_Visible(false);
			}
			ClearOrbitPlot();
		}

		private void DisplayGaiaStar()
		{
			//IL_00bd: Unknown result type (might be due to invalid IL or missing references)
			double Epoch = 0.0;
			bool UsedGaia = false;
			string Basis = "";
			string SourceFile = "None";
			string StarNumber = "";
			if (GetStarPosition.GetGaiaPosition(((Control)txtStar).get_Text(), out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR, out var Parallax_asec, out Epoch, out UsedGaia, out SourceFile, out StarNumber, out var TruncatedString, out var StarDiameter_mas))
			{
				if (((Control)txtStar).get_Text() != TruncatedString)
				{
					string text = ((Control)txtStar).get_Text();
					((Control)txtStar).set_Text(TruncatedString);
					MessageBox.Show("J coordinates for the star have been corrected from\r\n    '" + text + "'\r\n        to\r\n    '" + TruncatedString + "'\r\n\r\nThey need to be reviewed. The required format is\r\n\r\n hhmmss.ss-ddmmss.s  OR hhmmss.s-ddmmss\r\n\r\nwith '+' instead of '-' as needed", "J Coords corrected", (MessageBoxButtons)0, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				}
				RAstar = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
				DECstar = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				string text2 = "";
				if (UsedGaia)
				{
					text2 = "    From " + SourceFile;
				}
				int NumMeasures;
				bool InvalidDiameter;
				double num = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
				if (num > 1.0)
				{
					StarDiameter_mas = num;
				}
				else
				{
					StarDiameter_mas /= 1000.0;
					Basis = "Gaia DR3";
					NumMeasures = 0;
				}
				((Control)labelID).set_Text("J " + ((Control)txtStar).get_Text() + "   = " + StarNumber);
				((Control)labelRA).set_Text(" RA =  " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false) + "   PM = " + string.Format("{0,5:F5}s", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0) + text2);
				((Control)labelDec).set_Text("Dec = " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false) + "    PM = " + string.Format("{0,4:F4}\"", pmDec * (180.0 / Math.PI) * 3600.0));
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append($" MagV = {MagV:F2}" + $", MagB = {MagR:F2}" + string.Format(",  Parallax {0,5:f1}mas", Parallax_asec * 1000.0));
				if (StarDiameter_mas > 0.0)
				{
					if (NumMeasures > 0)
					{
						stringBuilder.Append($"    Dia: {StarDiameter_mas:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures));
					}
					else
					{
						stringBuilder.Append($"    Dia: {StarDiameter_mas:.0000}\"" + " [" + Basis + "]");
					}
				}
				else if (UsedGaia)
				{
					stringBuilder.Append("    Dia < 0.0001\"");
				}
				((Control)labelMag).set_Text(stringBuilder.ToString());
				DisplayKepler2ID(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
				RAforDouble = RA * (180.0 / Math.PI) / 15.0;
				DecForDouble = Dec * (180.0 / Math.PI);
			}
			else
			{
				((Control)labelID).set_Text("G " + ((Control)txtStar).get_Text() + " not found");
				((Control)labelRA).set_Text(" RA =  ?   PM = ?");
				((Control)labelDec).set_Text("Dec = ?   PM = ?");
				((Control)labelMag).set_Text(" Mag = ?, Mp = ?, Parallax = ?");
				RAforDouble = (DecForDouble = 0.0);
				((Control)lblKepler2).set_Visible(false);
			}
			ClearOrbitPlot();
		}

		private void DisplayUCAC4Star()
		{
			double num = 0.0;
			double Epoch = 2000.0;
			string Basis = "";
			string SourceFile = "None";
			bool UsedGaia = false;
			if (!int.TryParse(((Control)txtTzone).get_Text(), out var result))
			{
				result = -1;
			}
			if (result < 0 || result > 900)
			{
				return;
			}
			if (!int.TryParse(((Control)txtT2).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 < 1)
			{
				return;
			}
			string text = "4UCAC " + result.ToString().PadLeft(3, '0') + "-" + result2.ToString().PadLeft(6, '0');
			if (GetStarPosition.GetUCAC4Position(result, result2, Parallax_IfNotInGaia_TryHipparcos: false, out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR, out var Parallax_asec, out Epoch, out UsedGaia, out var _, out var _, out SourceFile, out var _, out var _, out var _, out var _, out var _, out var StarDiameter_mas, out var _, out var _, out var _, out var _))
			{
				RAstar = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
				DECstar = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				int NumMeasures;
				bool InvalidDiameter;
				double num2 = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
				if (num2 > 1.0)
				{
					num = num2;
				}
				else
				{
					num = StarDiameter_mas / 1000.0;
					Basis = "Gaia DR3";
					NumMeasures = 0;
				}
				string hipNum = UCAC4.HipNum;
				string tycho2Num = UCAC4.Tycho2Num;
				if (hipNum.Length > 1)
				{
					text = text + " = " + hipNum;
				}
				if (tycho2Num.Length > 2)
				{
					text = text + " = " + tycho2Num;
				}
				string text2 = "";
				if (UsedGaia)
				{
					text2 = "    From " + SourceFile;
				}
				((Control)labelID).set_Text(text);
				((Control)labelRA).set_Text(" RA =  " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false) + "   PM = " + string.Format("{0,5:F5}s", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0) + text2);
				((Control)labelDec).set_Text("Dec = " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false) + "    PM = " + string.Format("{0,4:F4}\"", pmDec * (180.0 / Math.PI) * 3600.0) + "     Epoch = " + string.Format("{0,5:F2}", Epoch));
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append($" MagV = {MagV:F2}" + $", MagB = {MagB:F2}" + $", MagR = {MagR:F2}" + $"     Parallax {Parallax_asec:F4}\"");
				if (num > 0.0)
				{
					if (NumMeasures > 0)
					{
						stringBuilder.Append($"    Dia: {num:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures));
					}
					else
					{
						stringBuilder.Append($"    Dia: {num:.0000}\"" + " [" + Basis + "]");
					}
				}
				else if (UsedGaia)
				{
					stringBuilder.Append("    Dia < 0.0001\"");
				}
				((Control)labelMag).set_Text(stringBuilder.ToString());
				DisplayKepler2ID(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
				RAforDouble = RA * (180.0 / Math.PI) / 15.0;
				DecForDouble = Dec * (180.0 / Math.PI);
			}
			else
			{
				((Control)labelID).set_Text(text + " not found");
				((Control)labelRA).set_Text(" RA =  ?   PM = ?");
				((Control)labelDec).set_Text("Dec = ?   PM = ?");
				((Control)labelMag).set_Text(" Mag = ?, Mp = ?");
				RAforDouble = (DecForDouble = 0.0);
				((Control)lblKepler2).set_Visible(false);
			}
			ClearOrbitPlot();
		}

		private void DisplayPPMXLStar()
		{
			string Basis = "";
			string text = ((Control)txtTzone).get_Text().Trim();
			bool flag = true;
			if (text.Length != 4)
			{
				return;
			}
			flag &= "ns".Contains(text.Substring(0, 1));
			flag &= "abcd".Contains(text.Substring(3, 1));
			if (!int.TryParse(text.Substring(1, 2), out var result))
			{
				flag = false;
			}
			if (result < 0 || result > 89 || !flag)
			{
				return;
			}
			if (!int.TryParse(((Control)txtT2).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 < 1)
			{
				return;
			}
			string text2 = text + "-" + result2.ToString().PadLeft(7, '0');
			if (GetStarPosition.GetPPMXLPosition(text, result2, out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR))
			{
				RAstar = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
				DECstar = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				int NumMeasures;
				bool InvalidDiameter;
				double num = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
				((Control)labelID).set_Text(text2);
				((Control)labelRA).set_Text(" RA =  " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false) + "   PM = " + string.Format("{0,5:F5}s", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0));
				((Control)labelDec).set_Text("Dec = " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false) + "    PM = " + string.Format("{0,4:F4}\"", pmDec * (180.0 / Math.PI) * 3600.0));
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append($" MagV = {MagV:F2}" + $", MagB = {MagB:F2}" + $", MagR = {MagR:F2}");
				if (NumMeasures > 0)
				{
					stringBuilder.Append($"    Dia: {num:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures));
				}
				((Control)labelMag).set_Text(stringBuilder.ToString());
				DisplayKepler2ID(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
				RAforDouble = RA * (180.0 / Math.PI) / 15.0;
				DecForDouble = Dec * (180.0 / Math.PI);
			}
			else
			{
				((Control)labelID).set_Text(text2 + " not found");
				((Control)labelRA).set_Text(" RA =  ?   PM = ?");
				((Control)labelDec).set_Text("Dec = ?   PM = ?");
				((Control)labelMag).set_Text(" Mag = ?, Mp = ?");
				RAforDouble = (DecForDouble = 0.0);
				((Control)lblKepler2).set_Visible(false);
			}
			ClearOrbitPlot();
		}

		private void DisplayB1Star()
		{
			string Basis = "";
			if (!int.TryParse(((Control)txtTzone).get_Text(), out var result))
			{
				result = -1;
			}
			if (result < 0 || result > 1799)
			{
				return;
			}
			if (!int.TryParse(((Control)txtT2).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 < 1)
			{
				return;
			}
			string text = "1B " + result + "-" + result2;
			if (GetStarPosition.GetB1Position(result, result2, out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _))
			{
				RAstar = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
				DECstar = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				int NumMeasures;
				bool InvalidDiameter;
				double num = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
				((Control)labelID).set_Text(text);
				((Control)labelRA).set_Text(" RA =  " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false) + "   PM = " + string.Format("{0,5:F5}s", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0));
				((Control)labelDec).set_Text("Dec = " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false) + "    PM = " + string.Format("{0,4:F4}\"", pmDec * (180.0 / Math.PI) * 3600.0));
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append($" MagV = {MagV:F2}" + $", MagB = {MagB:F2}" + $", MagR = {MagR:F2}");
				if (NumMeasures > 0)
				{
					stringBuilder.Append($"    Dia: {num:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures));
				}
				((Control)labelMag).set_Text(stringBuilder.ToString());
				DisplayKepler2ID(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
				RAforDouble = RA * (180.0 / Math.PI) / 15.0;
				DecForDouble = Dec * (180.0 / Math.PI);
			}
			else
			{
				((Control)labelID).set_Text(text + " not found");
				((Control)labelRA).set_Text(" RA =  ?   PM = ?");
				((Control)labelDec).set_Text("Dec = ?   PM = ?");
				((Control)labelMag).set_Text(" Mag = ?, Mp = ?");
				RAforDouble = (DecForDouble = 0.0);
				((Control)lblKepler2).set_Visible(false);
			}
			ClearOrbitPlot();
		}

		private void DisplayNOMADStar()
		{
			string Basis = "";
			if (!int.TryParse(((Control)txtTzone).get_Text(), out var result))
			{
				result = -1;
			}
			if (result < 0 || result > 1799)
			{
				return;
			}
			if (!int.TryParse(((Control)txtT2).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (result2 < 1)
			{
				return;
			}
			string text = "1N " + result + "-" + result2;
			if (GetStarPosition.GetNOMAD_Full_Position(result, result2, out var RA, out var Dec, out var pmRA, out var pmDec, out var MagV, out var MagB, out var MagR, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _))
			{
				RAstar = Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false);
				DECstar = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				int NumMeasures;
				bool InvalidDiameter;
				double num = Utilities.StarDiameter_CHARM2_CADARS(RA * (180.0 / Math.PI) / 15.0, Dec * (180.0 / Math.PI), MagV, MagB, MagR, out Basis, out NumMeasures, out InvalidDiameter);
				((Control)labelID).set_Text(text);
				((Control)labelRA).set_Text(" RA =  " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 4, MinutesOnly: false) + "   PM = " + string.Format("{0,5:F5}s", pmRA * (180.0 / Math.PI) / 15.0 * 3600.0));
				((Control)labelDec).set_Text("Dec = " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false) + "    PM = " + string.Format("{0,4:F4}\"", pmDec * (180.0 / Math.PI) * 3600.0));
				StringBuilder stringBuilder = new StringBuilder();
				stringBuilder.Append($" MagV = {MagV:F2}" + $", MagB = {MagB:F2}" + $", MagR = {MagR:F2}");
				if (NumMeasures > 0)
				{
					stringBuilder.Append($"    Dia: {num:.0000}\"" + " [" + Basis + string.Format(", {0,1} measures]", NumMeasures));
				}
				((Control)labelMag).set_Text(stringBuilder.ToString());
				DisplayKepler2ID(RA * (180.0 / Math.PI), Dec * (180.0 / Math.PI));
				RAforDouble = RA * (180.0 / Math.PI) / 15.0;
				DecForDouble = Dec * (180.0 / Math.PI);
			}
			else
			{
				((Control)labelID).set_Text(text + " not found");
				((Control)labelRA).set_Text(" RA =  ?   PM = ?");
				((Control)labelDec).set_Text("Dec = ?   PM = ?");
				((Control)labelMag).set_Text(" Mag = ?, Mp = ?");
				RAforDouble = (DecForDouble = 0.0);
				((Control)lblKepler2).set_Visible(false);
			}
			ClearOrbitPlot();
		}

		private void optGaia_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(true);
			((Control)txtStar).set_Width(125);
			((Control)lblJ).set_Visible(true);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtTzone).Focus();
		}

		private void optTyc_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(false);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = true);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = true);
			((Control)obj2).set_Visible(visible);
			((Control)txtTzone).Focus();
		}

		private void optZC_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(true);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtStar).Focus();
		}

		private void optSAO_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(true);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtStar).Focus();
		}

		private void optXZ_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(true);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtStar).Focus();
		}

		private void optHip_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(true);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = false);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtStar).Focus();
		}

		private void optUCAC4_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(false);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = true);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtTzone).Focus();
		}

		private void optPPMXL_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(false);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = true);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtTzone).Focus();
		}

		private void optB1_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(false);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = true);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtTzone).Focus();
		}

		private void optNOMAD_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtStar).set_Visible(false);
			((Control)txtStar).set_Width(72);
			((Control)lblJ).set_Visible(false);
			TextBox obj = txtTzone;
			bool visible;
			((Control)txtT2).set_Visible(visible = true);
			((Control)obj).set_Visible(visible);
			TextBox obj2 = txtT3;
			((Control)label11).set_Visible(visible = false);
			((Control)obj2).set_Visible(visible);
			((Control)txtTzone).Focus();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectDetails());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectDetails());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectDetails());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			string fileRootName = "";
			if (optTyc.get_Checked())
			{
				fileRootName = "TYC " + ((Control)txtTzone).get_Text().Trim() + "-" + ((Control)txtT2).get_Text().Trim();
			}
			else if (optZC.get_Checked())
			{
				fileRootName = "ZC " + ((Control)txtStar).get_Text().Trim();
			}
			else if (optSAO.get_Checked())
			{
				fileRootName = "SAO " + ((Control)txtStar).get_Text().Trim();
			}
			else if (optXZ.get_Checked())
			{
				fileRootName = "XZ " + ((Control)txtStar).get_Text().Trim();
			}
			else if (optHip.get_Checked())
			{
				fileRootName = "Hip " + ((Control)txtStar).get_Text().Trim();
			}
			else if (optB1.get_Checked())
			{
				fileRootName = "B1 " + ((Control)txtTzone).get_Text().Trim().PadLeft(4, '0') + "-" + ((Control)txtT2).get_Text().Trim().PadLeft(7, '0');
			}
			else if (optNOMAD.get_Checked())
			{
				fileRootName = "NOMAD " + ((Control)txtTzone).get_Text().Trim().PadLeft(4, '0') + "-" + ((Control)txtT2).get_Text().Trim().PadLeft(7, '0');
			}
			Settings.Default.Save_Stars = Output.SaveAppendPredictionText(CollectDetails(), fileRootName, Settings.Default.Save_Stars);
		}

		private string CollectDetails()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.AppendLine("Catalogue details");
			stringBuilder.AppendLine("");
			stringBuilder.AppendLine(((Control)labelID).get_Text());
			stringBuilder.AppendLine(((Control)labelRA).get_Text());
			stringBuilder.AppendLine(((Control)labelDec).get_Text());
			stringBuilder.AppendLine(((Control)labelMag).get_Text());
			if (((Control)grpVariables).get_Enabled())
			{
				stringBuilder.AppendLine("");
				stringBuilder.AppendLine(((Control)labelVariable).get_Text());
			}
			if (((Control)grpDoubles).get_Enabled())
			{
				stringBuilder.AppendLine("");
				stringBuilder.AppendLine(((Control)labelDouble).get_Text());
				stringBuilder.AppendLine(((Control)label2).get_Text());
				for (int i = 0; i < lstDoubles.get_Items().get_Count(); i++)
				{
					stringBuilder.AppendLine(lstDoubles.get_Items().get_Item(i).ToString());
				}
				stringBuilder.AppendLine("");
				stringBuilder.AppendLine(((Control)label1).get_Text());
				for (int j = 0; j < lstDiscoverers.get_Items().get_Count(); j++)
				{
					stringBuilder.AppendLine(lstDiscoverers.get_Items().get_Item(j).ToString());
				}
				if (lstOrbits.get_Items().get_Count() > 0)
				{
					stringBuilder.AppendLine("");
					stringBuilder.AppendLine("Orbital elements");
					for (int k = 0; k < lstOrbits.get_Items().get_Count(); k++)
					{
						stringBuilder.AppendLine(((Control)label4).get_Text());
						stringBuilder.AppendLine(lstOrbits.get_Items().get_Item(k).ToString());
						stringBuilder.AppendLine("Year   PA    Sep");
						for (int l = 0; l < 5; l++)
						{
							switch (k)
							{
							case 0:
								stringBuilder.AppendLine(lstEphem1.get_Items().get_Item(l).ToString());
								break;
							case 1:
								stringBuilder.AppendLine(lstEphem2.get_Items().get_Item(l).ToString());
								break;
							default:
								stringBuilder.AppendLine(lstEphem3.get_Items().get_Item(l).ToString());
								break;
							}
						}
						stringBuilder.AppendLine("");
					}
				}
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Stars - display details");
		}

		private void cmdShowWDS_IF_Click(object sender, EventArgs e)
		{
			//IL_003b: Unknown result type (might be due to invalid IL or missing references)
			if (!File.Exists(Utilities.AppPath + "\\DownLoaded Files\\InterferometricCat.dat") & !File.Exists(Utilities.AppPath + "\\DownLoaded Files\\wds.dat"))
			{
				MessageBox.Show("This option requires one or both of the Washington Double Star catalogue\r\n and the Interferometric Catalogue. Neither is present. \r\n\r\nDownload the catalogues using the Maintenance  Downloads   page", "No Catalogue");
			}
			else
			{
				Interferometric_Plus_WDS.Find_WDS_IF_Matches(RAforDouble, DecForDouble, HighPrecision: false, ShowResults: true, out WDS_Interferometer_Display.VariableID);
			}
		}

		private void cmdDisplayOrbits_Click(object sender, EventArgs e)
		{
			if (Interferometric_Plus_WDS.Orbits.Count >= 1)
			{
				Interferometric_Plus_WDS.ShowDisplayOrbit();
				Interferometric_Plus_WDS.DisplayOrbit.InitialDisplay();
			}
		}

		private void txtStar_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtStar).SelectAll();
		}

		private void txtStar_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtStar).SelectAll();
		}

		private void txtTzone_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtTzone).SelectAll();
		}

		private void txtTzone_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtTzone).SelectAll();
		}

		private void txtT2_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtT2).SelectAll();
		}

		private void txtStar_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.get_KeyChar() == '\r')
			{
				GetStar();
			}
			else
			{
				e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()) && e.get_KeyChar() != '.' && e.get_KeyChar() != '-');
			}
		}

		private void txtT3_KeyPress(object sender, KeyPressEventArgs e)
		{
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsNumber(e.get_KeyChar()) && e.get_KeyChar() != '.' && e.get_KeyChar() != '-');
		}

		private void txtT2_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.get_KeyChar() == '\r')
			{
				GetStar();
			}
			e.set_Handled(!char.IsControl(e.get_KeyChar()) && !char.IsNumber(e.get_KeyChar()) && e.get_KeyChar() != '.' && e.get_KeyChar() != '-');
		}

		private void txtTzone_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (e.get_KeyChar() == '\r')
			{
				((Control)txtT2).Focus();
				((TextBoxBase)txtT2).set_SelectionLength(0);
			}
			if (!char.IsControl(e.get_KeyChar()) && !char.IsNumber(e.get_KeyChar()) && e.get_KeyChar() != '.' && e.get_KeyChar() != '-')
			{
				e.set_Handled(true);
			}
		}

		private void cmdGaiaDoubles_Click(object sender, EventArgs e)
		{
			try
			{
				GaiaDoubles.Elements.Show_GaiaDoubles();
			}
			catch
			{
				GaiaDoubles.Elements.GaiaDoubles = new GaiaDoubles_Display();
				((Control)GaiaDoubles.Elements.GaiaDoubles).Show();
			}
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAh).set_Text(RAstar.Substring(0, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAm).set_Text(RAstar.Substring(3, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtRAs).set_Text(RAstar.Substring(6, 5));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecD).set_Text(DECstar.Substring(0, 3));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecM).set_Text(DECstar.Substring(4, 2));
			((Control)GaiaDoubles.Elements.GaiaDoubles.txtDecS).set_Text(DECstar.Substring(7, 4));
			GaiaDoubles.Elements.GaiaDoubles.DisplayDoubles_Within15arcmin();
		}

		private void txtT2_MouseClick(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtTzone).SelectAll();
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
			//IL_05d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e2: Expected O, but got Unknown
			//IL_0606: Unknown result type (might be due to invalid IL or missing references)
			//IL_0610: Expected O, but got Unknown
			//IL_086a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0874: Expected O, but got Unknown
			//IL_0f11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f1b: Expected O, but got Unknown
			//IL_0f3f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f49: Expected O, but got Unknown
			//IL_0fa7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fb1: Expected O, but got Unknown
			//IL_0fd5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fdf: Expected O, but got Unknown
			//IL_285f: Unknown result type (might be due to invalid IL or missing references)
			//IL_2869: Expected O, but got Unknown
			//IL_28d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_28da: Expected O, but got Unknown
			groupBox1 = new GroupBox();
			txtStar = new TextBox();
			lblJ = new Label();
			label14 = new Label();
			label13 = new Label();
			label12 = new Label();
			txtT3 = new TextBox();
			optNOMAD = new RadioButton();
			optHip = new RadioButton();
			optGaia = new RadioButton();
			optPPMXL = new RadioButton();
			optUCAC4 = new RadioButton();
			optB1 = new RadioButton();
			optTyc = new RadioButton();
			optXZ = new RadioButton();
			cmdGetStar = new Button();
			optSAO = new RadioButton();
			optZC = new RadioButton();
			txtT2 = new TextBox();
			txtTzone = new TextBox();
			label10 = new Label();
			label11 = new Label();
			labelRA = new Label();
			labelDec = new Label();
			labelMag = new Label();
			labelID = new Label();
			labelVariable = new Label();
			lstDoubles = new ListBox();
			lstDiscoverers = new ListBox();
			label1 = new Label();
			labelDouble = new Label();
			label2 = new Label();
			grpDoubles = new GroupBox();
			cmdDisplayOrbits = new Button();
			label9 = new Label();
			label8 = new Label();
			label7 = new Label();
			lstEphem3 = new ListBox();
			lstEphem2 = new ListBox();
			lstEphem1 = new ListBox();
			label6 = new Label();
			label4 = new Label();
			label3 = new Label();
			lstOrbits = new ListBox();
			grpVariables = new GroupBox();
			grpOrbit = new GroupBox();
			lblPASepn = new Label();
			cmdReDraw = new Button();
			cmdToday = new Button();
			label5 = new Label();
			updnYear = new NumericUpDown();
			picOrbit = new PictureBox();
			menuStrip1 = new MenuStrip();
			withDetailsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			cmdShowWDS_IF = new Button();
			lblKepler2 = new Label();
			cmdGaiaDoubles = new Button();
			label15 = new Label();
			((Control)groupBox1).SuspendLayout();
			((Control)grpDoubles).SuspendLayout();
			((Control)grpVariables).SuspendLayout();
			((Control)grpOrbit).SuspendLayout();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)picOrbit).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)groupBox1).get_Controls().Add((Control)(object)txtStar);
			((Control)groupBox1).get_Controls().Add((Control)(object)lblJ);
			((Control)groupBox1).get_Controls().Add((Control)(object)label14);
			((Control)groupBox1).get_Controls().Add((Control)(object)label13);
			((Control)groupBox1).get_Controls().Add((Control)(object)label12);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtT3);
			((Control)groupBox1).get_Controls().Add((Control)(object)optNOMAD);
			((Control)groupBox1).get_Controls().Add((Control)(object)optHip);
			((Control)groupBox1).get_Controls().Add((Control)(object)optGaia);
			((Control)groupBox1).get_Controls().Add((Control)(object)optPPMXL);
			((Control)groupBox1).get_Controls().Add((Control)(object)optUCAC4);
			((Control)groupBox1).get_Controls().Add((Control)(object)optB1);
			((Control)groupBox1).get_Controls().Add((Control)(object)optTyc);
			((Control)groupBox1).get_Controls().Add((Control)(object)optXZ);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdGetStar);
			((Control)groupBox1).get_Controls().Add((Control)(object)optSAO);
			((Control)groupBox1).get_Controls().Add((Control)(object)optZC);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtT2);
			((Control)groupBox1).get_Controls().Add((Control)(object)txtTzone);
			((Control)groupBox1).get_Controls().Add((Control)(object)label10);
			((Control)groupBox1).get_Controls().Add((Control)(object)label11);
			((Control)groupBox1).get_Controls().Add((Control)(object)label15);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(10, 29));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(296, 132));
			((Control)groupBox1).set_TabIndex(0);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Star");
			((Control)txtStar).set_Location(new Point(38, 16));
			((Control)txtStar).set_Name("txtStar");
			((Control)txtStar).set_Size(new Size(72, 20));
			((Control)txtStar).set_TabIndex(14);
			txtStar.set_TextAlign((HorizontalAlignment)2);
			((TextBoxBase)txtStar).add_MouseClick(new MouseEventHandler(txtStar_MouseClick));
			((Control)txtStar).add_Enter((EventHandler)txtStar_Enter);
			((Control)txtStar).add_KeyPress(new KeyPressEventHandler(txtStar_KeyPress));
			((Control)lblJ).set_AutoSize(true);
			((Control)lblJ).set_Location(new Point(24, 19));
			((Control)lblJ).set_Name("lblJ");
			((Control)lblJ).set_Size(new Size(13, 13));
			((Control)lblJ).set_TabIndex(23);
			((Control)lblJ).set_Text("J");
			((Control)lblJ).set_Visible(false);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(154, 41));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(89, 13));
			((Control)label14).set_TabIndex(22);
			((Control)label14).set_Text("Other catalogs");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(61, 41));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(63, 13));
			((Control)label13).set_TabIndex(21);
			((Control)label13).set_Text("Asteroidal");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold | FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(8, 41));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(39, 13));
			((Control)label12).set_TabIndex(20);
			((Control)label12).set_Text("Lunar");
			((Control)txtT3).set_Location(new Point(133, 16));
			((Control)txtT3).set_Name("txtT3");
			((Control)txtT3).set_Size(new Size(19, 20));
			((Control)txtT3).set_TabIndex(18);
			((Control)txtT3).set_Text("1");
			((Control)txtT3).set_Visible(false);
			((Control)txtT3).add_KeyPress(new KeyPressEventHandler(txtT3_KeyPress));
			((Control)optNOMAD).set_AutoSize(true);
			((Control)optNOMAD).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNOMAD).set_Location(new Point(150, 72));
			((Control)optNOMAD).set_Name("optNOMAD");
			((Control)optNOMAD).set_Size(new Size(65, 16));
			((Control)optNOMAD).set_TabIndex(7);
			((Control)optNOMAD).set_Text("NOMAD");
			((ButtonBase)optNOMAD).set_UseVisualStyleBackColor(true);
			optNOMAD.add_CheckedChanged((EventHandler)optNOMAD_CheckedChanged);
			((Control)optHip).set_AutoSize(true);
			((Control)optHip).set_Location(new Point(61, 55));
			((Control)optHip).set_Name("optHip");
			((Control)optHip).set_Size(new Size(82, 17));
			((Control)optHip).set_TabIndex(3);
			((Control)optHip).set_Text("Hipparcos");
			((ButtonBase)optHip).set_UseVisualStyleBackColor(true);
			optHip.add_CheckedChanged((EventHandler)optHip_CheckedChanged);
			((Control)optGaia).set_AutoSize(true);
			((Control)optGaia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optGaia).set_ForeColor(Color.MidnightBlue);
			((Control)optGaia).set_Location(new Point(61, 107));
			((Control)optGaia).set_Name("optGaia");
			((Control)optGaia).set_Size(new Size(129, 17));
			((Control)optGaia).set_TabIndex(9);
			((Control)optGaia).set_Text("Gaia - by coord ID");
			((ButtonBase)optGaia).set_UseVisualStyleBackColor(true);
			optGaia.add_CheckedChanged((EventHandler)optGaia_CheckedChanged);
			((Control)optPPMXL).set_AutoSize(true);
			((Control)optPPMXL).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optPPMXL).set_Location(new Point(150, 55));
			((Control)optPPMXL).set_Name("optPPMXL");
			((Control)optPPMXL).set_Size(new Size(60, 16));
			((Control)optPPMXL).set_TabIndex(6);
			((Control)optPPMXL).set_Text("PPMXL");
			((ButtonBase)optPPMXL).set_UseVisualStyleBackColor(true);
			optPPMXL.add_CheckedChanged((EventHandler)optPPMXL_CheckedChanged);
			((Control)optUCAC4).set_AutoSize(true);
			((Control)optUCAC4).set_Location(new Point(61, 89));
			((Control)optUCAC4).set_Name("optUCAC4");
			((Control)optUCAC4).set_Size(new Size(65, 17));
			((Control)optUCAC4).set_TabIndex(5);
			((Control)optUCAC4).set_Text("UCAC4");
			((ButtonBase)optUCAC4).set_UseVisualStyleBackColor(true);
			optUCAC4.add_CheckedChanged((EventHandler)optUCAC4_CheckedChanged);
			((Control)optB1).set_AutoSize(true);
			((Control)optB1).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optB1).set_Location(new Point(150, 89));
			((Control)optB1).set_Name("optB1");
			((Control)optB1).set_Size(new Size(71, 16));
			((Control)optB1).set_TabIndex(8);
			((Control)optB1).set_Text("USNO-B1");
			((ButtonBase)optB1).set_UseVisualStyleBackColor(true);
			optB1.add_CheckedChanged((EventHandler)optB1_CheckedChanged);
			((Control)optTyc).set_AutoSize(true);
			((Control)optTyc).set_Location(new Point(61, 72));
			((Control)optTyc).set_Name("optTyc");
			((Control)optTyc).set_Size(new Size(71, 17));
			((Control)optTyc).set_TabIndex(4);
			((Control)optTyc).set_Text("Tycho-2");
			((ButtonBase)optTyc).set_UseVisualStyleBackColor(true);
			optTyc.add_CheckedChanged((EventHandler)optTyc_CheckedChanged);
			((Control)optXZ).set_AutoSize(true);
			((Control)optXZ).set_Location(new Point(8, 89));
			((Control)optXZ).set_Name("optXZ");
			((Control)optXZ).set_Size(new Size(41, 17));
			((Control)optXZ).set_TabIndex(2);
			((Control)optXZ).set_Text("XZ");
			((ButtonBase)optXZ).set_UseVisualStyleBackColor(true);
			optXZ.add_CheckedChanged((EventHandler)optXZ_CheckedChanged);
			((Control)cmdGetStar).set_Location(new Point(173, 12));
			((Control)cmdGetStar).set_Name("cmdGetStar");
			((Control)cmdGetStar).set_Size(new Size(101, 28));
			((Control)cmdGetStar).set_TabIndex(17);
			((Control)cmdGetStar).set_Text("&Display details");
			((ButtonBase)cmdGetStar).set_UseVisualStyleBackColor(true);
			((Control)cmdGetStar).add_Click((EventHandler)cmdGetStar_Click);
			((Control)optSAO).set_AutoSize(true);
			((Control)optSAO).set_Location(new Point(8, 72));
			((Control)optSAO).set_Name("optSAO");
			((Control)optSAO).set_Size(new Size(50, 17));
			((Control)optSAO).set_TabIndex(1);
			((Control)optSAO).set_Text("SAO");
			((ButtonBase)optSAO).set_UseVisualStyleBackColor(true);
			optSAO.add_CheckedChanged((EventHandler)optSAO_CheckedChanged);
			((Control)optZC).set_AutoSize(true);
			optZC.set_Checked(true);
			((Control)optZC).set_Location(new Point(8, 55));
			((Control)optZC).set_Name("optZC");
			((Control)optZC).set_Size(new Size(41, 17));
			((Control)optZC).set_TabIndex(0);
			optZC.set_TabStop(true);
			((Control)optZC).set_Text("ZC");
			((ButtonBase)optZC).set_UseVisualStyleBackColor(true);
			optZC.add_CheckedChanged((EventHandler)optZC_CheckedChanged);
			((Control)txtT2).set_Location(new Point(67, 16));
			((Control)txtT2).set_Name("txtT2");
			((Control)txtT2).set_Size(new Size(59, 20));
			((Control)txtT2).set_TabIndex(15);
			((Control)txtT2).set_Visible(false);
			((TextBoxBase)txtT2).add_MouseClick(new MouseEventHandler(txtT2_MouseClick));
			((Control)txtT2).add_Enter((EventHandler)txtT2_Enter);
			((Control)txtT2).add_KeyPress(new KeyPressEventHandler(txtT2_KeyPress));
			((Control)txtTzone).set_Location(new Point(26, 16));
			((Control)txtTzone).set_Name("txtTzone");
			((Control)txtTzone).set_Size(new Size(34, 20));
			((Control)txtTzone).set_TabIndex(13);
			((Control)txtTzone).set_Visible(false);
			((TextBoxBase)txtTzone).add_MouseClick(new MouseEventHandler(txtTzone_MouseClick));
			((Control)txtTzone).add_Enter((EventHandler)txtTzone_Enter);
			((Control)txtTzone).add_KeyPress(new KeyPressEventHandler(txtTzone_KeyPress));
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(58, 19));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(11, 13));
			((Control)label10).set_TabIndex(16);
			((Control)label10).set_Text("-");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(124, 19));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(11, 13));
			((Control)label11).set_TabIndex(19);
			((Control)label11).set_Text("-");
			((Control)label11).set_Visible(false);
			((Control)labelRA).set_AutoSize(true);
			((Control)labelRA).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelRA).set_Location(new Point(332, 61));
			((Control)labelRA).set_Name("labelRA");
			((Control)labelRA).set_Size(new Size(21, 16));
			((Control)labelRA).set_TabIndex(3);
			((Control)labelRA).set_Text("RA");
			((Control)labelDec).set_AutoSize(true);
			((Control)labelDec).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelDec).set_Location(new Point(332, 88));
			((Control)labelDec).set_Name("labelDec");
			((Control)labelDec).set_Size(new Size(28, 16));
			((Control)labelDec).set_TabIndex(4);
			((Control)labelDec).set_Text("Dec");
			((Control)labelMag).set_AutoSize(true);
			((Control)labelMag).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelMag).set_Location(new Point(332, 115));
			((Control)labelMag).set_Name("labelMag");
			((Control)labelMag).set_Size(new Size(28, 16));
			((Control)labelMag).set_TabIndex(5);
			((Control)labelMag).set_Text("Mag");
			((Control)labelID).set_AutoSize(true);
			((Control)labelID).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelID).set_Location(new Point(332, 34));
			((Control)labelID).set_Name("labelID");
			((Control)labelID).set_Size(new Size(35, 16));
			((Control)labelID).set_TabIndex(6);
			((Control)labelID).set_Text("Star");
			((Control)labelVariable).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelVariable).set_Location(new Point(12, 18));
			((Control)labelVariable).set_Name("labelVariable");
			((Control)labelVariable).set_Size(new Size(355, 40));
			((Control)labelVariable).set_TabIndex(0);
			((Control)labelVariable).set_Text("Var");
			((Control)lstDoubles).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDoubles).set_FormattingEnabled(true);
			lstDoubles.set_ItemHeight(14);
			((Control)lstDoubles).set_Location(new Point(11, 50));
			((Control)lstDoubles).set_Name("lstDoubles");
			lstDoubles.set_ScrollAlwaysVisible(true);
			((Control)lstDoubles).set_Size(new Size(641, 88));
			((Control)lstDoubles).set_TabIndex(2);
			((Control)lstDiscoverers).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstDiscoverers).set_FormattingEnabled(true);
			lstDiscoverers.set_ItemHeight(14);
			((Control)lstDiscoverers).set_Location(new Point(8, 253));
			((Control)lstDiscoverers).set_Name("lstDiscoverers");
			lstDiscoverers.set_ScrollAlwaysVisible(true);
			((Control)lstDiscoverers).set_Size(new Size(249, 74));
			((Control)lstDiscoverers).set_TabIndex(7);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(11, 237));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(147, 14));
			((Control)label1).set_TabIndex(6);
			((Control)label1).set_Text("WDS Discoverer codes");
			((Control)labelDouble).set_AutoSize(true);
			((Control)labelDouble).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)labelDouble).set_Location(new Point(12, 16));
			((Control)labelDouble).set_Name("labelDouble");
			((Control)labelDouble).set_Size(new Size(49, 16));
			((Control)labelDouble).set_TabIndex(0);
			((Control)labelDouble).set_Text("Double");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Courier New", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(12, 35));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(623, 15));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text("Name    Cmpt   Yr1  Yr2: PA1   PA2  :   Sep1   Sep2  :  Mag1  Mag2:     #1      #2  mean");
			((Control)grpDoubles).get_Controls().Add((Control)(object)cmdDisplayOrbits);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label9);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label8);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label7);
			((Control)grpDoubles).get_Controls().Add((Control)(object)lstEphem3);
			((Control)grpDoubles).get_Controls().Add((Control)(object)lstEphem2);
			((Control)grpDoubles).get_Controls().Add((Control)(object)lstEphem1);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label6);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label4);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label3);
			((Control)grpDoubles).get_Controls().Add((Control)(object)lstOrbits);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label2);
			((Control)grpDoubles).get_Controls().Add((Control)(object)labelDouble);
			((Control)grpDoubles).get_Controls().Add((Control)(object)label1);
			((Control)grpDoubles).get_Controls().Add((Control)(object)lstDiscoverers);
			((Control)grpDoubles).get_Controls().Add((Control)(object)lstDoubles);
			((Control)grpDoubles).set_Enabled(false);
			((Control)grpDoubles).set_Location(new Point(324, 233));
			((Control)grpDoubles).set_Name("grpDoubles");
			((Control)grpDoubles).set_Size(new Size(663, 339));
			((Control)grpDoubles).set_TabIndex(14);
			grpDoubles.set_TabStop(false);
			((Control)grpDoubles).set_Text("Double star details");
			((Control)cmdDisplayOrbits).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDisplayOrbits).set_Location(new Point(587, 178));
			((Control)cmdDisplayOrbits).set_Name("cmdDisplayOrbits");
			((Control)cmdDisplayOrbits).set_Size(new Size(70, 33));
			((Control)cmdDisplayOrbits).set_TabIndex(15);
			((Control)cmdDisplayOrbits).set_Text("Display\r\nobservations");
			((ButtonBase)cmdDisplayOrbits).set_UseVisualStyleBackColor(true);
			((Control)cmdDisplayOrbits).add_Click((EventHandler)cmdDisplayOrbits_Click);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Courier New", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(527, 239));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(119, 14));
			((Control)label9).set_TabIndex(13);
			((Control)label9).set_Text("Year   PA    Sep");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Courier New", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(395, 239));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(119, 14));
			((Control)label8).set_TabIndex(11);
			((Control)label8).set_Text("Year   PA    Sep");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Courier New", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(263, 239));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(119, 14));
			((Control)label7).set_TabIndex(9);
			((Control)label7).set_Text("Year   PA    Sep");
			((Control)lstEphem3).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEphem3).set_FormattingEnabled(true);
			lstEphem3.set_ItemHeight(14);
			((Control)lstEphem3).set_Location(new Point(526, 253));
			((Control)lstEphem3).set_Name("lstEphem3");
			((Control)lstEphem3).set_Size(new Size(127, 74));
			((Control)lstEphem3).set_TabIndex(14);
			((Control)lstEphem2).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEphem2).set_FormattingEnabled(true);
			lstEphem2.set_ItemHeight(14);
			((Control)lstEphem2).set_Location(new Point(394, 253));
			((Control)lstEphem2).set_Name("lstEphem2");
			((Control)lstEphem2).set_Size(new Size(127, 74));
			((Control)lstEphem2).set_TabIndex(12);
			((Control)lstEphem1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstEphem1).set_FormattingEnabled(true);
			lstEphem1.set_ItemHeight(14);
			((Control)lstEphem1).set_Location(new Point(262, 253));
			((Control)lstEphem1).set_Name("lstEphem1");
			((Control)lstEphem1).set_Size(new Size(127, 74));
			((Control)lstEphem1).set_TabIndex(10);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(289, 224));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(336, 16));
			((Control)label6).set_TabIndex(8);
			((Control)label6).set_Text("O r b i t s :   5 - y e a r   e p h e m e r i s");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(11, 156));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(546, 14));
			((Control)label4).set_TabIndex(4);
			((Control)label4).set_Text("Name  Cmpt     Period       a\"        i       node      T        e       peri");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(13, 141));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(49, 16));
			((Control)label3).set_TabIndex(3);
			((Control)label3).set_Text("Orbits");
			((Control)lstOrbits).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstOrbits).set_FormattingEnabled(true);
			lstOrbits.set_ItemHeight(14);
			((Control)lstOrbits).set_Location(new Point(11, 171));
			((Control)lstOrbits).set_Name("lstOrbits");
			((Control)lstOrbits).set_Size(new Size(570, 46));
			((Control)lstOrbits).set_TabIndex(5);
			lstOrbits.add_SelectedIndexChanged((EventHandler)lstOrbits_SelectedIndexChanged);
			((Control)grpVariables).get_Controls().Add((Control)(object)labelVariable);
			((Control)grpVariables).set_Enabled(false);
			((Control)grpVariables).set_Location(new Point(324, 159));
			((Control)grpVariables).set_Name("grpVariables");
			((Control)grpVariables).set_Size(new Size(663, 72));
			((Control)grpVariables).set_TabIndex(15);
			grpVariables.set_TabStop(false);
			((Control)grpVariables).set_Text("Variable star details");
			((Control)grpOrbit).get_Controls().Add((Control)(object)lblPASepn);
			((Control)grpOrbit).get_Controls().Add((Control)(object)cmdReDraw);
			((Control)grpOrbit).get_Controls().Add((Control)(object)cmdToday);
			((Control)grpOrbit).get_Controls().Add((Control)(object)label5);
			((Control)grpOrbit).get_Controls().Add((Control)(object)updnYear);
			((Control)grpOrbit).get_Controls().Add((Control)(object)picOrbit);
			((Control)grpOrbit).set_Enabled(false);
			((Control)grpOrbit).set_Location(new Point(10, 167));
			((Control)grpOrbit).set_Name("grpOrbit");
			((Control)grpOrbit).set_Size(new Size(296, 405));
			((Control)grpOrbit).set_TabIndex(16);
			grpOrbit.set_TabStop(false);
			((Control)grpOrbit).set_Text("Double star orbit");
			((Control)lblPASepn).set_AutoSize(true);
			((Control)lblPASepn).set_Location(new Point(8, 65));
			((Control)lblPASepn).set_Name("lblPASepn");
			((Control)lblPASepn).set_Size(new Size(111, 13));
			((Control)lblPASepn).set_TabIndex(4);
			((Control)lblPASepn).set_Text("PA =  x.x°   Sep = s.s\"");
			((Control)cmdReDraw).set_Location(new Point(129, 28));
			((Control)cmdReDraw).set_Name("cmdReDraw");
			((Control)cmdReDraw).set_Size(new Size(66, 22));
			((Control)cmdReDraw).set_TabIndex(2);
			((Control)cmdReDraw).set_Text("ReDraw");
			((ButtonBase)cmdReDraw).set_UseVisualStyleBackColor(true);
			((Control)cmdReDraw).add_Click((EventHandler)cmdReDraw_Click);
			((Control)cmdToday).set_Location(new Point(216, 28));
			((Control)cmdToday).set_Name("cmdToday");
			((Control)cmdToday).set_Size(new Size(66, 22));
			((Control)cmdToday).set_TabIndex(3);
			((Control)cmdToday).set_Text("Use today");
			((ButtonBase)cmdToday).set_UseVisualStyleBackColor(true);
			((Control)cmdToday).add_Click((EventHandler)cmdToday_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(9, 30));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(29, 13));
			((Control)label5).set_TabIndex(0);
			((Control)label5).set_Text("Year");
			updnYear.set_DecimalPlaces(2);
			((Control)updnYear).set_Location(new Point(44, 28));
			updnYear.set_Maximum(new decimal(new int[4] { 9999, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(64, 20));
			((Control)updnYear).set_TabIndex(1);
			updnYear.set_Value(new decimal(new int[4] { 2007, 0, 0, 0 }));
			picOrbit.set_BorderStyle((BorderStyle)2);
			((Control)picOrbit).set_Location(new Point(8, 83));
			((Control)picOrbit).set_Name("picOrbit");
			((Control)picOrbit).set_Size(new Size(280, 308));
			picOrbit.set_TabIndex(0);
			picOrbit.set_TabStop(false);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withDetailsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(999, 24));
			((Control)menuStrip1).set_TabIndex(17);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withDetailsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withDetailsToolStripMenuItem).set_Name("withDetailsToolStripMenuItem");
			((ToolStripItem)withDetailsToolStripMenuItem).set_Size(new Size(104, 20));
			((ToolStripItem)withDetailsToolStripMenuItem).set_Text("with Details...     ");
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
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
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
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)cmdShowWDS_IF).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdShowWDS_IF).set_Location(new Point(830, 64));
			((Control)cmdShowWDS_IF).set_Name("cmdShowWDS_IF");
			((Control)cmdShowWDS_IF).set_Size(new Size(140, 37));
			((Control)cmdShowWDS_IF).set_TabIndex(24);
			((Control)cmdShowWDS_IF).set_Text("WDS, IF && AAVSO details");
			((ButtonBase)cmdShowWDS_IF).set_UseVisualStyleBackColor(true);
			((Control)cmdShowWDS_IF).add_Click((EventHandler)cmdShowWDS_IF_Click);
			((Control)lblKepler2).set_AutoSize(true);
			((Control)lblKepler2).set_Font(new Font("Courier New", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblKepler2).set_Location(new Point(332, 142));
			((Control)lblKepler2).set_Name("lblKepler2");
			((Control)lblKepler2).set_Size(new Size(147, 16));
			((Control)lblKepler2).set_TabIndex(25);
			((Control)lblKepler2).set_Text("Kepler2 target, ID =");
			((Control)lblKepler2).set_Visible(false);
			((Control)cmdGaiaDoubles).set_Location(new Point(852, 33));
			((Control)cmdGaiaDoubles).set_Name("cmdGaiaDoubles");
			((Control)cmdGaiaDoubles).set_Size(new Size(97, 25));
			((Control)cmdGaiaDoubles).set_TabIndex(26);
			((Control)cmdGaiaDoubles).set_Text("Gaia doubles");
			((ButtonBase)cmdGaiaDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdGaiaDoubles).add_Click((EventHandler)cmdGaiaDoubles_Click);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_ForeColor(Color.MidnightBlue);
			((Control)label15).set_Location(new Point(188, 102));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(103, 26));
			((Control)label15).set_TabIndex(24);
			((Control)label15).set_Text("closest star within 2\"\r\nwith mag diff <2.0");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(999, 584));
			((Control)this).get_Controls().Add((Control)(object)cmdGaiaDoubles);
			((Control)this).get_Controls().Add((Control)(object)lblKepler2);
			((Control)this).get_Controls().Add((Control)(object)cmdShowWDS_IF);
			((Control)this).get_Controls().Add((Control)(object)grpOrbit);
			((Control)this).get_Controls().Add((Control)(object)grpVariables);
			((Control)this).get_Controls().Add((Control)(object)grpDoubles);
			((Control)this).get_Controls().Add((Control)(object)labelID);
			((Control)this).get_Controls().Add((Control)(object)labelMag);
			((Control)this).get_Controls().Add((Control)(object)labelDec);
			((Control)this).get_Controls().Add((Control)(object)labelRA);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationStarsCatalogueDtails", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationStarsCatalogueDtails);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(860, 570));
			((Control)this).set_Name("StarCatalogueDetails");
			((Control)this).set_Text("Catalogue details for stars");
			((Form)this).add_FormClosed(new FormClosedEventHandler(StarCatalogueDetails_FormClosed));
			((Form)this).add_Load((EventHandler)StarCatalogueDetails_Load);
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)grpDoubles).ResumeLayout(false);
			((Control)grpDoubles).PerformLayout();
			((Control)grpVariables).ResumeLayout(false);
			((Control)grpOrbit).ResumeLayout(false);
			((Control)grpOrbit).PerformLayout();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)picOrbit).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
