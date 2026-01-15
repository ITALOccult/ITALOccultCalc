using System;
using System.ComponentModel;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult
{
	public class PlanetEphemeris : Form
	{
		private readonly string AppPath;

		private int PlanetNo;

		private int EquinoxFlag = 2;

		private bool CancelFlag;

		private const double Radian = 180.0 / Math.PI;

		private readonly string[] Equinox;

		private readonly string[] Planets;

		private bool DateChanging;

		private IContainer components;

		private Panel panel3;

		private Label label2;

		private Label label1;

		private NumericUpDown updnEndMonth;

		private NumericUpDown updnEndYear;

		private NumericUpDown updnStartMonth;

		private NumericUpDown updnStartYear;

		private GroupBox grpPlanet;

		private RadioButton optSun;

		private RadioButton optPluto;

		private RadioButton optNeptune;

		private RadioButton optUranus;

		private RadioButton optSaturn;

		private RadioButton optJupiter;

		private RadioButton optMars;

		private RadioButton optVenus;

		private RadioButton optMercury;

		private Label label3;

		private NumericUpDown updnStep;

		private MenuStrip menuStrip1;

		private GroupBox groupBox1;

		private RadioButton radioButton3;

		private RadioButton radioButton2;

		private RadioButton radioButton1;

		private ListBox lstOut;

		private Button cmdCancel;

		private Button cmdCompute;

		private ToolStripMenuItem withPredictionToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveAppendToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private CheckBox chkHighPrecision;

		private Button cmdNow;

		private Button cmdThisYear;

		private Button cmdNextYear;

		private ToolStripMenuItem saveToolStripMenuItem;

		public PlanetEphemeris()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			Equinox = new string[3] { "Apparent position", "Mean equinox of date", "Equinox of J2000" };
			Planets = new string[10] { "Sun", "Mercury", "Venus", "EMB", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune", "Pluto" };
		}

		private void PlanetEphemeris_Load(object sender, EventArgs e)
		{
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			updnStartYear.set_Value((decimal)DateTime.Now.Year);
			updnStartMonth.set_Value((decimal)DateTime.Now.Month);
			updnEndYear.set_Value((decimal)DateTime.Now.Year);
			updnEndMonth.set_Value((decimal)DateTime.Now.Month);
			((Control)cmdThisYear).set_Text(DateTime.Now.Year.ToString());
			((Control)cmdNextYear).set_Text((DateTime.Now.Year + 1).ToString());
		}

		private void PlanetEphemeris_Resize(object sender, EventArgs e)
		{
			if (!((((Control)this).get_Height() < 200) | (((Control)this).get_Width() < 100)))
			{
				((Control)lstOut).set_Height(((Control)this).get_Height() - 176);
				((Control)lstOut).set_Width(((Control)this).get_Width() - 35);
			}
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void cmdCompute_Click(object sender, EventArgs e)
		{
			Compute();
		}

		private void Compute()
		{
			int num = 0;
			int num2 = 0;
			string text = "De  Pp  Ds";
			string text2 = "o   o   o";
			if (chkHighPrecision.get_Checked())
			{
				num2 = 2;
				text = " De     Pp     Ds";
				text2 = " o      o      o";
			}
			string text3 = "".PadRight(num2);
			string text4 = "    Date     Right Asc. " + text3 + "  Declination " + text3 + "  Distance  " + text3 + "  dia   mag Elong    I  %Ill Limb   " + text;
			string text5 = "year mth  d   h  m   s " + text3 + "     o  '   \"  " + text3 + "   AU  " + text3 + "        \"           o     o          o    " + text2;
			if (updnStartYear.get_Value() < 1m)
			{
				text4 = "  " + text4;
				text5 = " " + text5.Insert(4, " ");
			}
			double num3 = Utilities.JD_from_Date((int)updnStartYear.get_Value(), (int)updnStartMonth.get_Value(), 1.0);
			double num4 = Utilities.JD_from_Date((int)updnEndYear.get_Value(), (int)updnEndMonth.get_Value(), 31.0);
			double num5 = (double)updnStep.get_Value();
			JPL_DE.InitialiseDE_Ephemeris();
			StringBuilder stringBuilder = new StringBuilder();
			lstOut.get_Items().Clear();
			lstOut.get_Items().Add((object)Planets[PlanetNo]);
			lstOut.get_Items().Add((object)Equinox[EquinoxFlag]);
			lstOut.get_Items().Add((object)("Ephemeris: " + Utilities.EphemerisBasis(num3)));
			lstOut.get_Items().Add((object)"");
			if (PlanetNo != 0)
			{
				lstOut.get_Items().Add((object)text4);
				lstOut.get_Items().Add((object)text5);
			}
			else
			{
				lstOut.get_Items().Add((object)(text4.Substring(0, 56 + 3 * num2) + "     Po     Bo      Lo   Carrington"));
				lstOut.get_Items().Add((object)(text5.Substring(0, 56 + 3 * num2) + "     o      o       o    Rotation #"));
			}
			((Control)cmdCompute).set_Visible(false);
			CancelFlag = false;
			for (double num6 = num3; num6 <= num4; num6 += num5)
			{
				double num7 = Utilities.delta_T(num6) / 86400.0;
				Utilities.PlanetGeocentric(num6 + num7, PlanetNo, 0.0, EquinoxFlag, physicalFlag: true, out var _, out var _, out var _, out var RA, out var Dec, out var TrueDistance, out var Diameter_arcsec, out var Magnitude, out var Elongation, out var EW, out var PhaseAngle_deg, out var illumination, out var PAlimb_deg, out var Planetocentric_Latitude_deg, out var PAPole_deg, out var Planetocentric_SunLat_deg);
				if (PlanetNo == 0)
				{
					Diameter_arcsec *= 2.0;
				}
				stringBuilder = new StringBuilder();
				stringBuilder.Append(Utilities.Date_from_JD(num6, 0) + "  ");
				stringBuilder.Append(Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 2 + num2, MinutesOnly: false) + "  ");
				stringBuilder.Append(Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 1 + num2, MinutesOnly: false));
				if (num2 > 0)
				{
					stringBuilder.AppendFormat("  {0,11:F8}", TrueDistance);
				}
				else
				{
					stringBuilder.AppendFormat("  {0,9:F6}", TrueDistance);
				}
				stringBuilder.AppendFormat(" {0,6:F1}", Diameter_arcsec);
				if (PlanetNo != 0)
				{
					stringBuilder.AppendFormat("  {0,4:F1}", Magnitude);
					stringBuilder.AppendFormat(" {0,5:F1}", Elongation * (180.0 / Math.PI));
					stringBuilder.Append(EW);
					stringBuilder.AppendFormat(" {0,3:F0}", PhaseAngle_deg);
					stringBuilder.AppendFormat(" {0,5:F1}", illumination * 100.0);
					stringBuilder.AppendFormat(" {0,5:F1}", PAlimb_deg);
					if (chkHighPrecision.get_Checked())
					{
						stringBuilder.AppendFormat(" {0,6:F2}", Planetocentric_Latitude_deg);
						stringBuilder.AppendFormat(" {0,6:F2}", PAPole_deg);
						stringBuilder.AppendFormat(" {0,6:F2}", Planetocentric_SunLat_deg);
						if (PlanetNo == 6)
						{
							if (Math.Sign(Planetocentric_Latitude_deg) != Math.Sign(Planetocentric_SunLat_deg))
							{
								stringBuilder.Append(" rings unlit");
							}
							else
							{
								stringBuilder.Append(" ");
							}
						}
						else
						{
							stringBuilder.Append(" ");
						}
					}
					else
					{
						stringBuilder.AppendFormat(" {0,3:F0}", Planetocentric_Latitude_deg);
						stringBuilder.AppendFormat(" {0,3:F0}", PAPole_deg);
						stringBuilder.AppendFormat(" {0,3:F0}", Planetocentric_SunLat_deg);
						if (PlanetNo == 6)
						{
							if (Math.Sign(Planetocentric_Latitude_deg) != Math.Sign(Planetocentric_SunLat_deg))
							{
								stringBuilder.Append(" rings unlit");
							}
							else
							{
								stringBuilder.Append(" ");
							}
						}
						else
						{
							stringBuilder.Append(" ");
						}
					}
				}
				else
				{
					Utilities.Meridians(num6 + num7, 3, out var Long1_deg, out var _, out var B_deg, out var P_deg);
					if (P_deg > 180.0)
					{
						P_deg -= 360.0;
					}
					stringBuilder.AppendFormat("  {0,7:F2}", P_deg);
					stringBuilder.AppendFormat(" {0,6:F2}", B_deg);
					stringBuilder.AppendFormat(" {0,7:F2}", Long1_deg);
					double num8 = (num6 - 2398167.446) / 27.2753;
					int num9 = (int)Math.Floor(num8) + 1;
					double num10 = num8 + 10000.0;
					if (num10 % 1.0 > 0.9 && Long1_deg > 270.0)
					{
						num9++;
					}
					if (num10 % 1.0 < 0.1 && Long1_deg < 90.0)
					{
						num9--;
					}
					stringBuilder.AppendFormat(" {0,7:F0}", num9);
				}
				lstOut.get_Items().Add((object)stringBuilder.ToString());
				num++;
				if (num % 5 == 0)
				{
					lstOut.get_Items().Add((object)"");
				}
				if (CancelFlag)
				{
					break;
				}
			}
			((Control)cmdCompute).set_Visible(true);
			CancelFlag = false;
		}

		private void updnStartYear_ValueChanged(object sender, EventArgs e)
		{
			if (!DateChanging && updnEndYear.get_Value() < updnStartYear.get_Value())
			{
				updnEndYear.set_Value(updnStartYear.get_Value());
			}
		}

		private void updnEndYear_ValueChanged(object sender, EventArgs e)
		{
			if (!DateChanging && updnEndYear.get_Value() < updnStartYear.get_Value())
			{
				updnStartYear.set_Value(updnEndYear.get_Value());
			}
		}

		private void optSun_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 0;
			if (optSun.get_Checked())
			{
				Compute();
			}
		}

		private void optMercury_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 1;
			if (optMercury.get_Checked())
			{
				Compute();
			}
		}

		private void optVenus_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 2;
			if (optVenus.get_Checked())
			{
				Compute();
			}
		}

		private void optMars_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 4;
			if (optMars.get_Checked())
			{
				Compute();
			}
		}

		private void optJupiter_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 5;
			if (optJupiter.get_Checked())
			{
				Compute();
			}
		}

		private void optSaturn_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 6;
			if (optSaturn.get_Checked())
			{
				Compute();
			}
		}

		private void optUranus_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 7;
			if (optUranus.get_Checked())
			{
				Compute();
			}
		}

		private void optNeptune_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 8;
			if (optNeptune.get_Checked())
			{
				Compute();
			}
		}

		private void optPluto_CheckedChanged(object sender, EventArgs e)
		{
			PlanetNo = 9;
			if (optPluto.get_Checked())
			{
				Compute();
			}
		}

		private void radioButton1_CheckedChanged(object sender, EventArgs e)
		{
			EquinoxFlag = 2;
			if (radioButton1.get_Checked())
			{
				Compute();
			}
		}

		private void radioButton2_CheckedChanged(object sender, EventArgs e)
		{
			EquinoxFlag = 1;
			if (radioButton2.get_Checked())
			{
				Compute();
			}
		}

		private void radioButton3_CheckedChanged(object sender, EventArgs e)
		{
			EquinoxFlag = 0;
			if (radioButton3.get_Checked())
			{
				Compute();
			}
		}

		private string CollectPrediction()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstOut.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstOut.get_Items().get_Item(i).ToString());
			}
			return stringBuilder.ToString();
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectPrediction());
		}

		private void printPreviewToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintPreviewText(CollectPrediction());
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.PrintText(CollectPrediction());
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SavePredictionText(CollectPrediction(), Planets[PlanetNo] + " " + updnStartYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void saveAppendToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_EphemerisData = Output.SaveAppendPredictionText(CollectPrediction(), Planets[PlanetNo] + " " + updnStartYear.get_Value(), Settings.Default.Save_EphemerisData);
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void updnStartYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartYear).Select(0, 10);
		}

		private void updnStartMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStartMonth).Select(0, 10);
		}

		private void updnEndYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndYear).Select(0, 10);
		}

		private void updnEndMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnEndMonth).Select(0, 10);
		}

		private void updnStep_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnStep).Select(0, 10);
		}

		private void ChkHighPrecision_CheckedChanged(object sender, EventArgs e)
		{
			Compute();
		}

		private void updnStartMonth_ValueChanged(object sender, EventArgs e)
		{
			if (DateChanging)
			{
				return;
			}
			DateChanging = true;
			if (updnStartMonth.get_Value() == 13m)
			{
				NumericUpDown obj = updnStartYear;
				obj.set_Value(obj.get_Value() + 1m);
				updnStartMonth.set_Value(1m);
			}
			else if (updnStartMonth.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnStartYear;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnStartMonth.set_Value(12m);
			}
			if (updnStartYear.get_Value() >= updnEndYear.get_Value())
			{
				if (updnStartYear.get_Value() > updnEndYear.get_Value())
				{
					updnEndYear.set_Value(updnStartYear.get_Value());
					updnEndMonth.set_Value(updnStartMonth.get_Value());
				}
				else if (updnStartMonth.get_Value() > updnEndMonth.get_Value())
				{
					updnEndMonth.set_Value(updnStartMonth.get_Value());
				}
			}
			DateChanging = false;
		}

		private void updnEndMonth_ValueChanged(object sender, EventArgs e)
		{
			if (DateChanging)
			{
				return;
			}
			DateChanging = true;
			if (updnEndMonth.get_Value() == 13m)
			{
				NumericUpDown obj = updnEndYear;
				obj.set_Value(obj.get_Value() + 1m);
				updnEndMonth.set_Value(1m);
			}
			else if (updnEndMonth.get_Value() == 0m)
			{
				NumericUpDown obj2 = updnEndYear;
				obj2.set_Value(obj2.get_Value() - 1m);
				updnEndMonth.set_Value(12m);
			}
			if (updnEndYear.get_Value() <= updnStartYear.get_Value())
			{
				if (updnEndYear.get_Value() < updnStartYear.get_Value())
				{
					updnStartYear.set_Value(updnEndYear.get_Value());
					updnStartMonth.set_Value(updnEndMonth.get_Value());
				}
				else if (updnEndMonth.get_Value() < updnStartMonth.get_Value())
				{
					updnStartMonth.set_Value(updnEndMonth.get_Value());
				}
			}
			DateChanging = false;
		}

		private void cmdNow_Click(object sender, EventArgs e)
		{
			DateChanging = true;
			NumericUpDown obj = updnStartMonth;
			decimal value;
			updnEndMonth.set_Value(value = DateTime.Now.Month);
			obj.set_Value(value);
			NumericUpDown obj2 = updnEndYear;
			updnStartYear.set_Value(value = DateTime.Now.Year);
			obj2.set_Value(value);
			DateChanging = false;
		}

		private void cmdThisYear_Click(object sender, EventArgs e)
		{
			DateChanging = true;
			updnStartMonth.set_Value(1m);
			updnEndMonth.set_Value(12m);
			NumericUpDown obj = updnEndYear;
			decimal value;
			updnStartYear.set_Value(value = DateTime.Now.Year);
			obj.set_Value(value);
			DateChanging = false;
		}

		private void cmdNextYear_Click(object sender, EventArgs e)
		{
			DateChanging = true;
			updnStartMonth.set_Value(1m);
			updnEndMonth.set_Value(12m);
			NumericUpDown obj = updnEndYear;
			decimal value;
			updnStartYear.set_Value(value = DateTime.Now.Year + 1);
			obj.set_Value(value);
			DateChanging = false;
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Planet Ephemerides");
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
			//IL_00ed: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Expected O, but got Unknown
			//IL_00f8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0102: Expected O, but got Unknown
			//IL_0103: Unknown result type (might be due to invalid IL or missing references)
			//IL_010d: Expected O, but got Unknown
			//IL_010e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0118: Expected O, but got Unknown
			//IL_0119: Unknown result type (might be due to invalid IL or missing references)
			//IL_0123: Expected O, but got Unknown
			//IL_0124: Unknown result type (might be due to invalid IL or missing references)
			//IL_012e: Expected O, but got Unknown
			//IL_012f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0139: Expected O, but got Unknown
			//IL_013a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0144: Expected O, but got Unknown
			//IL_0145: Unknown result type (might be due to invalid IL or missing references)
			//IL_014f: Expected O, but got Unknown
			//IL_0150: Unknown result type (might be due to invalid IL or missing references)
			//IL_015a: Expected O, but got Unknown
			//IL_015b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0165: Expected O, but got Unknown
			//IL_0166: Unknown result type (might be due to invalid IL or missing references)
			//IL_0170: Expected O, but got Unknown
			//IL_0171: Unknown result type (might be due to invalid IL or missing references)
			//IL_017b: Expected O, but got Unknown
			//IL_017c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0186: Expected O, but got Unknown
			//IL_0187: Unknown result type (might be due to invalid IL or missing references)
			//IL_0191: Expected O, but got Unknown
			//IL_0192: Unknown result type (might be due to invalid IL or missing references)
			//IL_019c: Expected O, but got Unknown
			//IL_019d: Unknown result type (might be due to invalid IL or missing references)
			//IL_01a7: Expected O, but got Unknown
			//IL_01a8: Unknown result type (might be due to invalid IL or missing references)
			//IL_01b2: Expected O, but got Unknown
			//IL_01b3: Unknown result type (might be due to invalid IL or missing references)
			//IL_01bd: Expected O, but got Unknown
			//IL_0558: Unknown result type (might be due to invalid IL or missing references)
			//IL_0562: Expected O, but got Unknown
			//IL_192c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1936: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(PlanetEphemeris));
			panel3 = new Panel();
			cmdNextYear = new Button();
			cmdThisYear = new Button();
			cmdNow = new Button();
			label3 = new Label();
			updnStep = new NumericUpDown();
			label2 = new Label();
			label1 = new Label();
			updnEndMonth = new NumericUpDown();
			updnEndYear = new NumericUpDown();
			updnStartMonth = new NumericUpDown();
			updnStartYear = new NumericUpDown();
			grpPlanet = new GroupBox();
			optSun = new RadioButton();
			optPluto = new RadioButton();
			optNeptune = new RadioButton();
			optUranus = new RadioButton();
			optSaturn = new RadioButton();
			optJupiter = new RadioButton();
			optMars = new RadioButton();
			optVenus = new RadioButton();
			optMercury = new RadioButton();
			menuStrip1 = new MenuStrip();
			withPredictionToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			saveAppendToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			groupBox1 = new GroupBox();
			radioButton3 = new RadioButton();
			radioButton2 = new RadioButton();
			radioButton1 = new RadioButton();
			lstOut = new ListBox();
			cmdCancel = new Button();
			cmdCompute = new Button();
			chkHighPrecision = new CheckBox();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)updnStep).BeginInit();
			((ISupportInitialize)updnEndMonth).BeginInit();
			((ISupportInitialize)updnEndYear).BeginInit();
			((ISupportInitialize)updnStartMonth).BeginInit();
			((ISupportInitialize)updnStartYear).BeginInit();
			((Control)grpPlanet).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)panel3).set_Anchor((AnchorStyles)1);
			((Control)panel3).get_Controls().Add((Control)(object)cmdNextYear);
			((Control)panel3).get_Controls().Add((Control)(object)cmdThisYear);
			((Control)panel3).get_Controls().Add((Control)(object)cmdNow);
			((Control)panel3).get_Controls().Add((Control)(object)label3);
			((Control)panel3).get_Controls().Add((Control)(object)updnStep);
			((Control)panel3).get_Controls().Add((Control)(object)label2);
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnEndYear);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartMonth);
			((Control)panel3).get_Controls().Add((Control)(object)updnStartYear);
			((Control)panel3).set_Location(new Point(298, 39));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(262, 83));
			((Control)panel3).set_TabIndex(1);
			((Control)cmdNextYear).set_Location(new Point(214, 57));
			((Control)cmdNextYear).set_Name("cmdNextYear");
			((Control)cmdNextYear).set_Size(new Size(41, 21));
			((Control)cmdNextYear).set_TabIndex(10);
			((Control)cmdNextYear).set_Text("2000");
			((ButtonBase)cmdNextYear).set_UseVisualStyleBackColor(true);
			((Control)cmdNextYear).add_Click((EventHandler)cmdNextYear_Click);
			((Control)cmdThisYear).set_Location(new Point(214, 31));
			((Control)cmdThisYear).set_Name("cmdThisYear");
			((Control)cmdThisYear).set_Size(new Size(41, 21));
			((Control)cmdThisYear).set_TabIndex(9);
			((Control)cmdThisYear).set_Text("2000");
			((ButtonBase)cmdThisYear).set_UseVisualStyleBackColor(true);
			((Control)cmdThisYear).add_Click((EventHandler)cmdThisYear_Click);
			((Control)cmdNow).set_Location(new Point(214, 4));
			((Control)cmdNow).set_Name("cmdNow");
			((Control)cmdNow).set_Size(new Size(41, 21));
			((Control)cmdNow).set_TabIndex(8);
			((Control)cmdNow).set_Text("Now");
			((ButtonBase)cmdNow).set_UseVisualStyleBackColor(true);
			((Control)cmdNow).add_Click((EventHandler)cmdNow_Click);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(3, 59));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(124, 13));
			((Control)label3).set_TabIndex(6);
			((Control)label3).set_Text("Ephemeris interval [days]");
			((Control)updnStep).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "EphemerisInterval", true, (DataSourceUpdateMode)1));
			((Control)updnStep).set_Location(new Point(138, 57));
			updnStep.set_Maximum(new decimal(new int[4] { 366, 0, 0, 0 }));
			updnStep.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnStep).set_Name("updnStep");
			((Control)updnStep).set_Size(new Size(48, 20));
			((Control)updnStep).set_TabIndex(7);
			updnStep.set_Value(Settings.Default.EphemerisInterval);
			((Control)updnStep).add_Enter((EventHandler)updnStep_Enter);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(6, 33));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(90, 13));
			((Control)label2).set_TabIndex(3);
			((Control)label2).set_Text("End year && month");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(3, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(93, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Start year && month");
			((Control)updnEndMonth).set_Location(new Point(163, 31));
			updnEndMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnEndMonth).set_Name("updnEndMonth");
			((Control)updnEndMonth).set_Size(new Size(39, 20));
			((Control)updnEndMonth).set_TabIndex(5);
			updnEndMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnEndMonth.add_ValueChanged((EventHandler)updnEndMonth_ValueChanged);
			((Control)updnEndMonth).add_Enter((EventHandler)updnEndMonth_Enter);
			((Control)updnEndYear).set_Location(new Point(100, 31));
			updnEndYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnEndYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnEndYear).set_Name("updnEndYear");
			((Control)updnEndYear).set_Size(new Size(55, 20));
			((Control)updnEndYear).set_TabIndex(4);
			updnEndYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			updnEndYear.add_ValueChanged((EventHandler)updnEndYear_ValueChanged);
			((Control)updnEndYear).add_Enter((EventHandler)updnEndYear_Enter);
			((Control)updnStartMonth).set_Location(new Point(162, 4));
			updnStartMonth.set_Maximum(new decimal(new int[4] { 13, 0, 0, 0 }));
			((Control)updnStartMonth).set_Name("updnStartMonth");
			((Control)updnStartMonth).set_Size(new Size(40, 20));
			((Control)updnStartMonth).set_TabIndex(2);
			updnStartMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnStartMonth.add_ValueChanged((EventHandler)updnStartMonth_ValueChanged);
			((Control)updnStartMonth).add_Enter((EventHandler)updnStartMonth_Enter);
			((Control)updnStartYear).set_Location(new Point(100, 4));
			updnStartYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnStartYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnStartYear).set_Name("updnStartYear");
			((Control)updnStartYear).set_Size(new Size(55, 20));
			((Control)updnStartYear).set_TabIndex(1);
			updnStartYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			updnStartYear.add_ValueChanged((EventHandler)updnStartYear_ValueChanged);
			((Control)updnStartYear).add_Enter((EventHandler)updnStartYear_Enter);
			((Control)grpPlanet).set_Anchor((AnchorStyles)1);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optSun);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optPluto);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optNeptune);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optUranus);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optSaturn);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optJupiter);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optMars);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optVenus);
			((Control)grpPlanet).get_Controls().Add((Control)(object)optMercury);
			((Control)grpPlanet).set_Location(new Point(96, 41));
			((Control)grpPlanet).set_Name("grpPlanet");
			((Control)grpPlanet).set_Size(new Size(198, 81));
			((Control)grpPlanet).set_TabIndex(0);
			grpPlanet.set_TabStop(false);
			((Control)grpPlanet).set_Text("Planet");
			((Control)optSun).set_AutoSize(true);
			optSun.set_Checked(true);
			((Control)optSun).set_Location(new Point(6, 16));
			((Control)optSun).set_Name("optSun");
			((Control)optSun).set_Size(new Size(44, 17));
			((Control)optSun).set_TabIndex(0);
			optSun.set_TabStop(true);
			((Control)optSun).set_Text("Sun");
			((ButtonBase)optSun).set_UseVisualStyleBackColor(true);
			optSun.add_CheckedChanged((EventHandler)optSun_CheckedChanged);
			((Control)optPluto).set_AutoSize(true);
			((Control)optPluto).set_Location(new Point(131, 53));
			((Control)optPluto).set_Name("optPluto");
			((Control)optPluto).set_Size(new Size(49, 17));
			((Control)optPluto).set_TabIndex(8);
			((Control)optPluto).set_Text("Pluto");
			((ButtonBase)optPluto).set_UseVisualStyleBackColor(true);
			optPluto.add_CheckedChanged((EventHandler)optPluto_CheckedChanged);
			((Control)optNeptune).set_AutoSize(true);
			((Control)optNeptune).set_Location(new Point(131, 35));
			((Control)optNeptune).set_Name("optNeptune");
			((Control)optNeptune).set_Size(new Size(66, 17));
			((Control)optNeptune).set_TabIndex(7);
			((Control)optNeptune).set_Text("Neptune");
			((ButtonBase)optNeptune).set_UseVisualStyleBackColor(true);
			optNeptune.add_CheckedChanged((EventHandler)optNeptune_CheckedChanged);
			((Control)optUranus).set_AutoSize(true);
			((Control)optUranus).set_Location(new Point(131, 16));
			((Control)optUranus).set_Name("optUranus");
			((Control)optUranus).set_Size(new Size(59, 17));
			((Control)optUranus).set_TabIndex(6);
			((Control)optUranus).set_Text("Uranus");
			((ButtonBase)optUranus).set_UseVisualStyleBackColor(true);
			optUranus.add_CheckedChanged((EventHandler)optUranus_CheckedChanged);
			((Control)optSaturn).set_AutoSize(true);
			((Control)optSaturn).set_Location(new Point(71, 53));
			((Control)optSaturn).set_Name("optSaturn");
			((Control)optSaturn).set_Size(new Size(56, 17));
			((Control)optSaturn).set_TabIndex(5);
			((Control)optSaturn).set_Text("Saturn");
			((ButtonBase)optSaturn).set_UseVisualStyleBackColor(true);
			optSaturn.add_CheckedChanged((EventHandler)optSaturn_CheckedChanged);
			((Control)optJupiter).set_AutoSize(true);
			((Control)optJupiter).set_Location(new Point(71, 35));
			((Control)optJupiter).set_Name("optJupiter");
			((Control)optJupiter).set_Size(new Size(56, 17));
			((Control)optJupiter).set_TabIndex(4);
			((Control)optJupiter).set_Text("Jupiter");
			((ButtonBase)optJupiter).set_UseVisualStyleBackColor(true);
			optJupiter.add_CheckedChanged((EventHandler)optJupiter_CheckedChanged);
			((Control)optMars).set_AutoSize(true);
			((Control)optMars).set_Location(new Point(71, 16));
			((Control)optMars).set_Name("optMars");
			((Control)optMars).set_Size(new Size(48, 17));
			((Control)optMars).set_TabIndex(3);
			((Control)optMars).set_Text("Mars");
			((ButtonBase)optMars).set_UseVisualStyleBackColor(true);
			optMars.add_CheckedChanged((EventHandler)optMars_CheckedChanged);
			((Control)optVenus).set_AutoSize(true);
			((Control)optVenus).set_Location(new Point(6, 53));
			((Control)optVenus).set_Name("optVenus");
			((Control)optVenus).set_Size(new Size(55, 17));
			((Control)optVenus).set_TabIndex(2);
			((Control)optVenus).set_Text("Venus");
			((ButtonBase)optVenus).set_UseVisualStyleBackColor(true);
			optVenus.add_CheckedChanged((EventHandler)optVenus_CheckedChanged);
			((Control)optMercury).set_AutoSize(true);
			((Control)optMercury).set_Location(new Point(6, 35));
			((Control)optMercury).set_Name("optMercury");
			((Control)optMercury).set_Size(new Size(63, 17));
			((Control)optMercury).set_TabIndex(1);
			((Control)optMercury).set_Text("Mercury");
			((ButtonBase)optMercury).set_UseVisualStyleBackColor(true);
			optMercury.add_CheckedChanged((EventHandler)optMercury_CheckedChanged);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withPredictionToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(905, 24));
			((Control)menuStrip1).set_TabIndex(6);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withPredictionToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem,
				(ToolStripItem)saveAppendToolStripMenuItem
			});
			((ToolStripItem)withPredictionToolStripMenuItem).set_Name("withPredictionToolStripMenuItem");
			withPredictionToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)withPredictionToolStripMenuItem).set_Size(new Size(114, 20));
			((ToolStripItem)withPredictionToolStripMenuItem).set_Text("with Prediction...  ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			copyToolStripMenuItem.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("&Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Image((Image)Resources.PrintPreview);
			((ToolStripItem)printPreviewToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printPreviewToolStripMenuItem).set_Name("printPreviewToolStripMenuItem");
			((ToolStripItem)printPreviewToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)printPreviewToolStripMenuItem).set_Text("Print preview");
			((ToolStripItem)printPreviewToolStripMenuItem).add_Click((EventHandler)printPreviewToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			printToolStripMenuItem.set_ShortcutKeys((Keys)131152);
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("&Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			saveToolStripMenuItem.set_ShortcutKeys((Keys)131155);
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)saveAppendToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveAppendToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveAppendToolStripMenuItem).set_Name("saveAppendToolStripMenuItem");
			saveAppendToolStripMenuItem.set_ShortcutKeys((Keys)131137);
			((ToolStripItem)saveAppendToolStripMenuItem).set_Size(new Size(193, 22));
			((ToolStripItem)saveAppendToolStripMenuItem).set_Text("&Save (Append)");
			((ToolStripItem)saveAppendToolStripMenuItem).add_Click((EventHandler)saveAppendToolStripMenuItem_Click);
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
			((Control)groupBox1).set_Anchor((AnchorStyles)1);
			((Control)groupBox1).get_Controls().Add((Control)(object)radioButton3);
			((Control)groupBox1).get_Controls().Add((Control)(object)radioButton2);
			((Control)groupBox1).get_Controls().Add((Control)(object)radioButton1);
			((Control)groupBox1).set_Location(new Point(565, 42));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(148, 79));
			((Control)groupBox1).set_TabIndex(2);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Equinox for positions");
			((Control)radioButton3).set_AutoSize(true);
			((Control)radioButton3).set_Location(new Point(9, 59));
			((Control)radioButton3).set_Name("radioButton3");
			((Control)radioButton3).set_Size(new Size(108, 17));
			((Control)radioButton3).set_TabIndex(2);
			((Control)radioButton3).set_Text("Apparent equinox");
			((ButtonBase)radioButton3).set_UseVisualStyleBackColor(true);
			radioButton3.add_CheckedChanged((EventHandler)radioButton3_CheckedChanged);
			((Control)radioButton2).set_AutoSize(true);
			((Control)radioButton2).set_Location(new Point(9, 39));
			((Control)radioButton2).set_Name("radioButton2");
			((Control)radioButton2).set_Size(new Size(128, 17));
			((Control)radioButton2).set_TabIndex(1);
			((Control)radioButton2).set_Text("Mean equinox of date");
			((ButtonBase)radioButton2).set_UseVisualStyleBackColor(true);
			radioButton2.add_CheckedChanged((EventHandler)radioButton2_CheckedChanged);
			((Control)radioButton1).set_AutoSize(true);
			radioButton1.set_Checked(true);
			((Control)radioButton1).set_Location(new Point(9, 19));
			((Control)radioButton1).set_Name("radioButton1");
			((Control)radioButton1).set_Size(new Size(138, 17));
			((Control)radioButton1).set_TabIndex(0);
			radioButton1.set_TabStop(true);
			((Control)radioButton1).set_Text("J2000 standard equinox");
			((ButtonBase)radioButton1).set_UseVisualStyleBackColor(true);
			radioButton1.add_CheckedChanged((EventHandler)radioButton1_CheckedChanged);
			((Control)lstOut).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstOut).set_FormattingEnabled(true);
			lstOut.set_ItemHeight(14);
			((Control)lstOut).set_Location(new Point(12, 127));
			((Control)lstOut).set_Name("lstOut");
			((Control)lstOut).set_Size(new Size(886, 396));
			((Control)lstOut).set_TabIndex(5);
			((Control)cmdCancel).set_Anchor((AnchorStyles)1);
			((Control)cmdCancel).set_Location(new Point(720, 47));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(83, 30));
			((Control)cmdCancel).set_TabIndex(4);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdCompute).set_Anchor((AnchorStyles)1);
			((Control)cmdCompute).set_Location(new Point(721, 47));
			((Control)cmdCompute).set_Name("cmdCompute");
			((Control)cmdCompute).set_Size(new Size(81, 30));
			((Control)cmdCompute).set_TabIndex(3);
			((Control)cmdCompute).set_Text("Compute");
			((ButtonBase)cmdCompute).set_UseVisualStyleBackColor(true);
			((Control)cmdCompute).add_Click((EventHandler)cmdCompute_Click);
			((Control)chkHighPrecision).set_Anchor((AnchorStyles)1);
			((Control)chkHighPrecision).set_AutoSize(true);
			chkHighPrecision.set_Checked(true);
			chkHighPrecision.set_CheckState((CheckState)1);
			((Control)chkHighPrecision).set_Location(new Point(723, 100));
			((Control)chkHighPrecision).set_Name("chkHighPrecision");
			((Control)chkHighPrecision).set_Size(new Size(93, 17));
			((Control)chkHighPrecision).set_TabIndex(7);
			((Control)chkHighPrecision).set_Text("High precision");
			((ButtonBase)chkHighPrecision).set_UseVisualStyleBackColor(true);
			chkHighPrecision.add_CheckedChanged((EventHandler)ChkHighPrecision_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(905, 536));
			((Control)this).get_Controls().Add((Control)(object)chkHighPrecision);
			((Control)this).get_Controls().Add((Control)(object)cmdCompute);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)lstOut);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)grpPlanet);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationEphemPlanets", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationEphemPlanets);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("PlanetEphemeris");
			((Control)this).set_Text("Ephemeris of the Sun, Planets, and Pluto");
			((Form)this).add_Load((EventHandler)PlanetEphemeris_Load);
			((Control)this).add_Resize((EventHandler)PlanetEphemeris_Resize);
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)updnStep).EndInit();
			((ISupportInitialize)updnEndMonth).EndInit();
			((ISupportInitialize)updnEndYear).EndInit();
			((ISupportInitialize)updnStartMonth).EndInit();
			((ISupportInitialize)updnStartYear).EndInit();
			((Control)grpPlanet).ResumeLayout(false);
			((Control)grpPlanet).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
