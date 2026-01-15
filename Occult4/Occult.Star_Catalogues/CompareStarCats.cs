using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.Text;
using System.Windows.Forms;
using GifCreator;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class CompareStarCats : Form
	{
		private struct Star
		{
			public double RAatEpoch;

			public double DECatEpoch;

			public double pmRA;

			public double pmDEC;

			public double piRA;

			public double piDec;

			public double RAUncertAtEpoch;

			public double DecUncertaintyAtEpoch;
		}

		private const double Radian = 180.0 / Math.PI;

		private static double TargetRA = 0.0;

		private static double TargetDec = 0.0;

		private bool IsPlotting;

		private bool ShowProperMotions = true;

		private bool GetToolTip = true;

		internal bool FromAsteroidPrediction;

		private static double Duration;

		private static double Diameter;

		private static double PAMotion;

		private static Pen[] PenChords = new Pen[30];

		private List<StarEntry> stars = new List<StarEntry>();

		private VizierMirror Vizier;

		private ArrayList HTML = new ArrayList();

		private static float OldX = 0f;

		private static float OldY = 0f;

		private static bool GettingTag = false;

		internal static List<StarCatTags> Tags = new List<StarCatTags>();

		private static List<Star> Offsets = new List<Star>();

		private IContainer components;

		private TextBox txtTycComp;

		private TextBox txtTycSeqNum;

		private TextBox txtTycRegion;

		private Label label8;

		private Label label7;

		private TextBox txtHip;

		private Button cmdGetStarPos;

		private Label label10;

		private Label label9;

		private TextBox txtB1number;

		private TextBox txtB1zone;

		private Label label6;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label16;

		private Label label13;

		private Label label12;

		private Label label11;

		private Label label14;

		private Button cmdListPositions;

		private Label label15;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private CheckedListBox chklstCats;

		private Label label18;

		private ListBox lstPositions;

		private Label label17;

		private Label label19;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		internal ComboBox cmbCatalogue;

		internal Panel panelTycho2;

		internal Panel panelHip;

		internal Panel panelB1;

		private PictureBox picStarPlot;

		private PictureBox picLegend;

		private Label label20;

		private TrackBar trackBar1;

		private Label lblScale;

		private ToolStripMenuItem copyImageToolStripMenuItem;

		private ToolStripMenuItem copyLegendToolStripMenuItem;

		private ToolStripMenuItem saveImageLegendToolStripMenuItem;

		private Label label23;

		private Label label21;

		private Label label22;

		private ToolStripMenuItem whiteBackgroundToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator1;

		private Label label24;

		internal GroupBox grpStar;

		internal Panel panelCoordinates;

		internal Panel panelSelectCatalog;

		internal Label lblEventDetails;

		private ToolStripMenuItem saveFormAsAnImageToolStripMenuItem;

		private ToolStripSeparator toolStripSeparator2;

		private Button cmdReDisplay;

		private Button cmdNow;

		private ToolStripMenuItem animatedGifToolStripMenuItem;

		private ToolStripMenuItem setAnimatedGIFParametersToolStripMenuItem;

		private ToolStripMenuItem create10To5YearsToolStripMenuItem;

		private ToolStripMenuItem vizierSiteToolStripMenuItem;

		private ToolStripMenuItem franceToolStripMenuItem;

		private ToolStripMenuItem uSAToolStripMenuItem;

		private ToolStripMenuItem canadaToolStripMenuItem;

		private ToolStripMenuItem japanToolStripMenuItem;

		private ToolStripMenuItem indiaToolStripMenuItem;

		private ToolStripMenuItem chinaToolStripMenuItem;

		private ToolStripMenuItem uKToolStripMenuItem;

		private ToolStripMenuItem hawaiiToolStripMenuItem;

		private ToolStripMenuItem russiaToolStripMenuItem;

		internal NumericUpDown updnRadius;

		internal NumericUpDown updnEpoch;

		internal TextBox txts;

		internal TextBox txtm;

		internal TextBox txtD;

		internal TextBox txtRAs;

		internal TextBox txtRAm;

		internal TextBox txtRAh;

		internal NumericUpDown updnDiameter;

		internal NumericUpDown updnMotionPA;

		internal CheckBox chkShowMotion;

		internal TextBox txtMp;

		internal TextBox txtMv;

		private CheckBox chkScale;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem showStarInVizieRToolStripMenuItem;

		private Label label25;

		internal TextBox txtDuration;

		private Button cmdGoogleSky;

		private Button cmdDoubles;

		private Label label26;

		private ToolTip toolTip;

		internal Panel panelUCAC4;

		private TextBox txtUCNumber;

		private TextBox txtUCZone;

		private Label label27;

		private Label label28;

		private CheckBox chkStarUncertainty;

		private Label label29;

		private CheckBox chkParallax;

		public CompareStarCats(bool fromAsteroidPrediction)
		{
			InitializeComponent();
			FromAsteroidPrediction = fromAsteroidPrediction;
		}

		private void CompareStarCats_Load(object sender, EventArgs e)
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
			((Control)txtTycComp).set_Text("1");
			((ListControl)cmbCatalogue).set_SelectedIndex(0);
			chklstCats.SetItemCheckState(0, (CheckState)1);
			chklstCats.SetItemCheckState(1, (CheckState)1);
			chklstCats.SetItemCheckState(5, (CheckState)1);
			chklstCats.SetItemCheckState(7, (CheckState)1);
			chklstCats.SetItemCheckState(8, (CheckState)1);
			updnEpoch.set_Value((decimal)Utilities.BesselianYear(Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day)));
			((Control)panelCoordinates).set_Enabled(!FromAsteroidPrediction);
			((Control)panelSelectCatalog).set_Visible(!FromAsteroidPrediction);
			((Control)lblEventDetails).set_Visible(FromAsteroidPrediction);
			if (!FromAsteroidPrediction)
			{
				((Control)grpStar).set_Text("Star");
			}
			else
			{
				((Control)grpStar).set_Text("Event");
			}
			setVizierChecks(Settings.Default.VizierServer);
		}

		private void cmbCatalogue_SelectedIndexChanged(object sender, EventArgs e)
		{
			ShowNumberField(((ListControl)cmbCatalogue).get_SelectedIndex());
		}

		private void ShowNumberField(int x)
		{
			((Control)panelHip).set_Visible(x == 0);
			((Control)panelTycho2).set_Visible(x == 1);
			((Control)panelUCAC4).set_Visible(x == 2);
			((Control)panelB1).set_Visible(x == 3);
		}

		private void cmdGetStarPos_Click(object sender, EventArgs e)
		{
			//IL_00ee: Unknown result type (might be due to invalid IL or missing references)
			double RA = 0.0;
			double pmRA = 0.0;
			double Dec = 0.0;
			double pmDec = 0.0;
			double Parallax = 0.0;
			double MagV = 0.0;
			double MagB = 0.0;
			double MagR = 0.0;
			double Epoch = 2000.0;
			bool flag = false;
			bool UsedGaia = false;
			int result3;
			if (((ListControl)cmbCatalogue).get_SelectedIndex() == 0)
			{
				if (!int.TryParse(((Control)txtHip).get_Text(), out var result))
				{
					return;
				}
				flag = GetStarPosition.GetHipparcosPosition(result, out RA, out Dec, out pmRA, out pmDec, out MagR, out MagV, out MagB, out Parallax, out Epoch, out UsedGaia, out var CorrectStarIdentifier);
				if (!flag)
				{
					MessageBox.Show("HIP" + ((Control)txtHip).get_Text() + " is held in the Occult Gaia catalogue as\r\n" + CorrectStarIdentifier + "\r\nSet the star identifier to this star", "HIP star identifier", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 1)
			{
				if (!int.TryParse(((Control)txtTycRegion).get_Text(), out var result2) || !int.TryParse(((Control)txtTycSeqNum).get_Text(), out result3) || !int.TryParse(((Control)txtTycComp).get_Text(), out var result4))
				{
					return;
				}
				flag = GetStarPosition.GetTycho2Position(result2, result3, result4, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out Epoch);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 2)
			{
				if (!int.TryParse(((Control)txtUCZone).get_Text(), out var result5) || !int.TryParse(((Control)txtUCNumber).get_Text(), out var result6))
				{
					return;
				}
				flag = GetStarPosition.GetUCAC4Position(result5, result6, Parallax_IfNotInGaia_TryHipparcos: false, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out Parallax, out UsedGaia, out Epoch);
			}
			else if (((ListControl)cmbCatalogue).get_SelectedIndex() == 3)
			{
				if (!int.TryParse(((Control)txtB1zone).get_Text(), out var result7) || !int.TryParse(((Control)txtB1number).get_Text(), out result3))
				{
					return;
				}
				flag = GetStarPosition.GetB1Position(result7, result3, out RA, out Dec, out pmRA, out pmDec, out MagV, out MagB, out MagR, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
			}
			if (flag)
			{
				string text = Utilities.DEGtoDMS(RA / 15.0 * (180.0 / Math.PI), 2, 4, MinutesOnly: false);
				((Control)txtRAh).set_Text(text.Substring(0, 2));
				((Control)txtRAm).set_Text(text.Substring(3, 2));
				((Control)txtRAs).set_Text(text.Substring(6));
				text = Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 3, MinutesOnly: false);
				((Control)txtD).set_Text(text.Substring(0, 3));
				((Control)txtm).set_Text(text.Substring(4, 2));
				((Control)txts).set_Text(text.Substring(7));
				((Control)txtMv).set_Text(string.Format("{0,3:F2}", MagV));
				((Control)txtMp).set_Text(string.Format("{0,3:F2}", MagB));
			}
			else
			{
				TextBox obj = txtRAh;
				TextBox obj2 = txtRAm;
				string text2;
				((Control)txtRAs).set_Text(text2 = "");
				string SourceFile;
				((Control)obj2).set_Text(SourceFile = text2);
				((Control)obj).set_Text(SourceFile);
				TextBox obj3 = txtD;
				TextBox obj4 = txtm;
				((Control)txts).set_Text(text2 = "");
				((Control)obj4).set_Text(SourceFile = text2);
				((Control)obj3).set_Text(SourceFile);
				TextBox obj5 = txtMv;
				((Control)txtMp).set_Text(SourceFile = "");
				((Control)obj5).set_Text(SourceFile);
			}
		}

		private void cmdListPositions_Click(object sender, EventArgs e)
		{
			((Control)cmdReDisplay).set_Enabled(false);
			trackBar1.set_Value(20);
			FindInSpecificCatalogues(DownLoadVizier: true);
		}

		private void cmdReDisplay_Click(object sender, EventArgs e)
		{
			FindInSpecificCatalogues(DownLoadVizier: false);
		}

		internal void FindInSpecificCatalogues(bool DownLoadVizier)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_003b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0041: Invalid comparison between Unknown and I4
			string text = "";
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			Cursor.set_Current(Cursors.get_WaitCursor());
			if (DownLoadVizier)
			{
				for (int i = 0; i < ((ObjectCollection)chklstCats.get_Items()).get_Count(); i++)
				{
					if ((int)chklstCats.GetItemCheckState(i) == 1)
					{
						string text2 = ((ObjectCollection)chklstCats.get_Items()).get_Item(i).ToString()!.Substring(0, 8).Trim();
						text = ((text.Length != 0) ? (text + "," + text2) : text2);
					}
				}
				SetTargetLocation();
				stars = CatalogQuery.GetStarsInRegion(TargetRA / 15.0, TargetDec, (int)updnRadius.get_Value(), VizierEquinox.J2000, text, Vizier);
				((Control)cmdReDisplay).set_Enabled(true);
			}
			double num = (double)updnEpoch.get_Value();
			double jD = Utilities.JD_from_Date((int)Math.Floor(num), 1, 1.0) + 365.25 * (num - Math.Truncate(num));
			Diameter = (double)updnDiameter.get_Value();
			if (Diameter <= 0.0)
			{
				Diameter = 10.0;
			}
			if (!double.TryParse(((Control)txtDuration).get_Text(), out Duration))
			{
				Duration = 1.0;
			}
			if (Duration < 0.1)
			{
				Duration = 1.0;
			}
			PAMotion = (double)updnMotionPA.get_Value();
			Offsets.Clear();
			lstPositions.get_Items().Clear();
			lstPositions.get_Items().Add((object)"                              Magnitudes              current epoch          displacement    uncertainty in Right Asc.     uncertainty in Declination    Flags    catalogue details at epoch ...");
			lstPositions.get_Items().Add((object)("Cat                           B    V    R        R.A.  (" + string.Format("{0,7:f2}", num) + ")  Dec        Xpath  time    RA    pm.RA  epoch   total     Dec  pm.Dec  epoch   total    DVO #       R.A.         PM         epoch      Declination      PM       epoch    Plx   e_Plx   Notes"));
			lstPositions.get_Items().Add((object)"ID                                             h  m   s        o  '   \"      width   sec    \"     \"/yr    yr      \"        \"     \"/yr    yr      \"                h  m   s        s           yr         o  '   \"        \"         yr      \"      \" ");
			_ = (double)updnRadius.get_Value();
			double ParallaxCoeff_RA = 0.0;
			double ParallaxCoeff_Dec = 0.0;
			double rAStar = TargetRA / (180.0 / Math.PI);
			double decStar = TargetDec / (180.0 / Math.PI);
			if (chkParallax.get_Checked())
			{
				Utilities.StellarParallaxCoefficients(jD, rAStar, decStar, out ParallaxCoeff_RA, out ParallaxCoeff_Dec);
			}
			HTML.Clear();
			foreach (StarEntry star in stars)
			{
				if (!(star.CatalogId == "II/241"))
				{
					lstPositions.get_Items().Add((object)FormatStarLine(star, num, (double)updnMotionPA.get_Value() / (180.0 / Math.PI), ParallaxCoeff_RA, ParallaxCoeff_Dec));
					if (star.CatalogId == "I/259")
					{
						int num2 = star.StarName.IndexOf(" ");
						int num3 = star.StarName.IndexOf("-");
						int num4 = star.StarName.IndexOf("-", num3 + 1);
						HTML.Add("http://" + CatalogQuery.VIZIER_URLS[(int)Vizier] + "/cgi-bin/VizieR-5?-out.add=.&-source=" + star.CatalogId + "&TYC1===" + star.StarName.Substring(num2 + 1, num3 - num2 - 1) + "&TYC2===" + star.StarName.Substring(num3 + 1, num4 - num3 - 1) + "&TYC3===" + star.StarName.Substring(num4 + 1));
					}
					else
					{
						HTML.Add("http://" + CatalogQuery.VIZIER_URLS[(int)Vizier] + "/cgi-bin/VizieR-5?-out.add=.&-source=" + star.CatalogId + "&" + star.StarName.Replace(" ", "===").Replace("+", "%2b"));
					}
				}
			}
			DrawOffsets();
			lstPositions.get_Items().Add((object)"");
			lstPositions.get_Items().Add((object)"***  Search complete  ***");
			Cursor.set_Current(Cursors.get_Default());
		}

		internal static string FormatStarLine(StarEntry Star, double CurrentEpoch, double PAofMotion, double ParallaxCoeff_RA, double ParallaxCoeff_Dec)
		{
			double num = 0.0;
			double num2 = 0.0;
			string text = " ";
			Star item = default(Star);
			bool flag = (Star.CatalogId == "I/345") | (Star.CatalogId == "I/350");
			if (Star.StarName == null)
			{
				Star.StarName = "";
			}
			double num3 = ((Star.RACat.CompareTo(double.NaN) != 0) ? Star.RACat : 0.0);
			if (Star.pmRA.CompareTo(double.NaN) != 0)
			{
				num = (item.pmRA = Star.pmRA / 3600.0);
			}
			else
			{
				num = (item.pmRA = 0.0);
				text += "No PM  ";
			}
			double num4 = ((Star.DECat.CompareTo(double.NaN) != 0) ? Star.DECat : 0.0);
			num2 = ((Star.pmDEC.CompareTo(double.NaN) == 0) ? (item.pmDEC = 0.0) : (item.pmDEC = Star.pmDEC / 3600.0));
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(Star.StarName.PadRight(26).Substring(0, 26) + " ");
			if (Star.Mb != 0.0)
			{
				stringBuilder.AppendFormat("{0,5:f1}", Star.Mb);
			}
			else
			{
				stringBuilder.Append("  ...");
			}
			if (Star.Mv != 0.0)
			{
				stringBuilder.AppendFormat("{0,5:f1}", Star.Mv);
			}
			else
			{
				stringBuilder.Append("  ...");
			}
			if (Star.Mr != 0.0)
			{
				stringBuilder.AppendFormat("{0,5:f1}    ", Star.Mr);
			}
			else
			{
				stringBuilder.Append("  ...    ");
			}
			double num5 = CurrentEpoch - Star.EpochRA;
			double num6 = CurrentEpoch - Star.EpochDE;
			double parallax = Star.Parallax;
			item.piRA = 0.0;
			item.piDec = 0.0;
			if (parallax > 0.0)
			{
				item.piRA = parallax / 3600000.0 * ParallaxCoeff_RA / 15.0;
				item.piDec = parallax / 3600000.0 * ParallaxCoeff_Dec;
			}
			double num7 = (item.RAatEpoch = num3 + num5 * num + item.piRA);
			double num8 = (item.DECatEpoch = num4 + num6 * num2 + item.piDec);
			if (flag)
			{
				stringBuilder.Append(Utilities.DEGtoDMS(num7, 2, 5, MinutesOnly: false) + " ");
				stringBuilder.Append(Utilities.DEGtoDMS(num8, 3, 4, MinutesOnly: false) + " ");
			}
			else
			{
				stringBuilder.Append(Utilities.DEGtoDMS(num7, 2, 4, MinutesOnly: false) + "  ");
				stringBuilder.Append(Utilities.DEGtoDMS(num8, 3, 3, MinutesOnly: false) + "  ");
			}
			Utilities.Distance(num7 / (180.0 / Math.PI) * 15.0, num8 / (180.0 / Math.PI), TargetRA / (180.0 / Math.PI), TargetDec / (180.0 / Math.PI), out var Distance, out var PA_atOrigin);
			double num9 = PA_atOrigin - PAMotion / (180.0 / Math.PI);
			Distance *= 648000000.0 / Math.PI / Diameter;
			if (Distance * Math.Abs(Math.Sin(num9)) < 99.95)
			{
				stringBuilder.AppendFormat("{0,5:f2}", Distance * Math.Abs(Math.Sin(num9)));
			}
			else
			{
				stringBuilder.Append(">99.9");
			}
			double num10 = (double)Math.Sign(Math.Sin(num9)) * Math.Sin(PAofMotion);
			if (num10 > 0.3)
			{
				stringBuilder.Append("S");
			}
			else if (num10 < -0.3)
			{
				stringBuilder.Append("N");
			}
			else if (num10 > 0.0)
			{
				stringBuilder.Append("W");
			}
			else
			{
				stringBuilder.Append("E");
			}
			double num11 = (0.0 - Distance) * Duration * Math.Cos(num9);
			if (Math.Abs(num11) < 999.9)
			{
				stringBuilder.AppendFormat("{0,6:f1}", num11);
			}
			else
			{
				stringBuilder.AppendFormat("{0,6:f0}", num11);
			}
			if (double.IsNaN(Star.e_RA))
			{
				stringBuilder.Append("    ... ");
			}
			else
			{
				stringBuilder.AppendFormat("  {0,6:f3}", Star.e_RA);
			}
			if (double.IsNaN(Star.e_pmRA))
			{
				stringBuilder.Append("  ...  ");
			}
			else
			{
				stringBuilder.AppendFormat("{0,7:f4}", Star.e_pmRA);
			}
			stringBuilder.AppendFormat(" {0,7:f2}  ", Star.EpochErrorRA);
			if (double.IsNaN(Star.e_RA) | double.IsNaN(Star.e_pmRA))
			{
				if (double.IsNaN(Star.e_RA))
				{
					item.RAUncertAtEpoch = -1.0;
				}
				else
				{
					item.RAUncertAtEpoch = 0.0 - Star.e_RA;
				}
				stringBuilder.Append(" ...    ");
			}
			else
			{
				item.RAUncertAtEpoch = Math.Sqrt(Math.Pow(Star.e_RA, 2.0) + Math.Pow(Star.e_pmRA * (Star.EpochErrorRA - CurrentEpoch), 2.0));
				stringBuilder.AppendFormat("{0,5:f3}   ", item.RAUncertAtEpoch);
			}
			if (double.IsNaN(Star.e_DEC))
			{
				stringBuilder.Append("  ... ");
			}
			else
			{
				stringBuilder.AppendFormat("{0,6:f3}", Star.e_DEC);
			}
			if (double.IsNaN(Star.e_pmDEC))
			{
				stringBuilder.Append("  ...  ");
			}
			else
			{
				stringBuilder.AppendFormat("{0,7:f4}", Star.e_pmDEC);
			}
			stringBuilder.AppendFormat(" {0,7:f2}  ", Star.EpochErrorDE);
			if (double.IsNaN(Star.e_DEC) | double.IsNaN(Star.e_pmDEC))
			{
				if (double.IsNaN(Star.e_DEC))
				{
					item.DecUncertaintyAtEpoch = -1.0;
				}
				else
				{
					item.DecUncertaintyAtEpoch = 0.0 - Star.e_DEC;
				}
				stringBuilder.Append(" ...   ");
			}
			else
			{
				item.DecUncertaintyAtEpoch = Math.Sqrt(Math.Pow(Star.e_DEC, 2.0) + Math.Pow(Star.e_pmDEC * (Star.EpochErrorDE - CurrentEpoch), 2.0));
				stringBuilder.AppendFormat("{0,5:f3}  ", item.DecUncertaintyAtEpoch);
			}
			stringBuilder.Append("  " + Star.flag_Double + Star.flag_Variable + Star.flag_Other + Star.flag_NumCats);
			if (Star.RACat.CompareTo(double.NaN) == 0)
			{
				stringBuilder.Append("    -  -  -       ");
			}
			else if (flag)
			{
				stringBuilder.Append("   " + Utilities.DEGtoDMS(Star.RACat, 2, 5, MinutesOnly: false) + " ");
			}
			else
			{
				stringBuilder.Append("   " + Utilities.DEGtoDMS(Star.RACat, 2, 4, MinutesOnly: false) + "  ");
			}
			if (Star.pmRA.CompareTo(double.NaN) == 0)
			{
				stringBuilder.Append("  ...      ");
			}
			else if (flag)
			{
				stringBuilder.AppendFormat("{0,9:F6}  ", Star.pmRA);
			}
			else
			{
				stringBuilder.AppendFormat("{0,8:F5}   ", Star.pmRA);
			}
			stringBuilder.Append(string.Format("{0,6:f3}", Star.EpochRA).PadRight(10));
			stringBuilder.Append("  ");
			if (Star.DECat.CompareTo(double.NaN) == 0)
			{
				stringBuilder.Append("  -  -  -      ");
			}
			else if (flag)
			{
				stringBuilder.Append(Utilities.DEGtoDMS(Star.DECat, 3, 4, MinutesOnly: false) + " ");
			}
			else
			{
				stringBuilder.Append(Utilities.DEGtoDMS(Star.DECat, 3, 3, MinutesOnly: false) + "  ");
			}
			if (Star.pmDEC.CompareTo(double.NaN) == 0)
			{
				stringBuilder.Append("   ...    ");
			}
			else if (flag)
			{
				stringBuilder.AppendFormat("{0,9:F5} ", Star.pmDEC);
			}
			else
			{
				stringBuilder.AppendFormat("{0,8:F4}  ", Star.pmDEC);
			}
			stringBuilder.Append(string.Format("{0,6:f3}", Star.EpochDE).PadRight(10));
			if (Star.Parallax > 0.0)
			{
				stringBuilder.Append(string.Format("{0,6:f4} {1,6:f4}", Star.Parallax / 1000.0, Star.e_Parallax / 1000.0).PadRight(11));
			}
			else
			{
				stringBuilder.Append("".PadRight(13));
			}
			stringBuilder.Append(" " + text);
			Offsets.Add(item);
			return stringBuilder.ToString();
		}

		private string GetVizierLocationSpecification()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(((Control)txtRAh).get_Text().Trim() + " ");
			stringBuilder.Append(((Control)txtRAm).get_Text().Trim() + " ");
			stringBuilder.Append(((Control)txtRAs).get_Text().Trim() + " ");
			stringBuilder.Append(((Control)txtD).get_Text().Trim().Replace(" ", "") + " ");
			stringBuilder.Append(((Control)txtm).get_Text().Trim() + " ");
			stringBuilder.Append(((Control)txts).get_Text().Trim());
			return stringBuilder.ToString();
		}

		private void SetTargetLocation()
		{
			if (!int.TryParse(((Control)txtRAh).get_Text(), out var result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtRAm).get_Text(), out var result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txtRAs).get_Text(), out var result3))
			{
				result3 = 0.0;
			}
			TargetRA = 15.0 * ((double)result + (double)result2 / 60.0 + result3 / 3600.0);
			if (!int.TryParse(((Control)txtD).get_Text().Replace("-", ""), out result))
			{
				result = 0;
			}
			if (!int.TryParse(((Control)txtm).get_Text(), out result2))
			{
				result2 = 0;
			}
			if (!double.TryParse(((Control)txts).get_Text(), out result3))
			{
				result3 = 0.0;
			}
			TargetDec = (double)result + (double)result2 / 60.0 + result3 / 3600.0;
			if (((Control)txtD).get_Text().Contains("-"))
			{
				TargetDec = 0.0 - TargetDec;
			}
		}

		private void CompareStarCats_Resize(object sender, EventArgs e)
		{
			((Control)lstPositions).set_Width(((Control)this).get_Width() - 40);
			((Control)lstPositions).set_Height(((Control)this).get_Height() - 430);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectEvents());
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstPositions.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstPositions.get_Items().get_Item(i).ToString());
			}
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Compare Star Catalogues");
		}

		private void DrawOffsets()
		{
			if (!IsPlotting)
			{
				IsPlotting = true;
				int width = ((Control)picStarPlot).get_Width();
				int height = ((Control)picStarPlot).get_Height();
				if (!(width < 100 || height < 50))
				{
					Bitmap image = new Bitmap(width, height);
					Graphics graphics = Graphics.FromImage(image);
					PlotOffsets(graphics, width, height, Printer: false);
					picStarPlot.set_Image((Image)image);
					graphics.Dispose();
					IsPlotting = false;
				}
			}
		}

		private void PlotOffsets(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer)
		{
			double num = 0.0;
			double num2 = 0.0;
			float num3 = (float)updnDiameter.get_Value() / 2f;
			float num4 = 0f;
			float num5 = 0f;
			bool @checked = whiteBackgroundToolStripMenuItem.get_Checked();
			float num6 = (float)(ChartWidth / 200) / (1f + (float)trackBar1.get_Value() / 60f) / (1f + (float)trackBar1.get_Value() / 60f);
			if (chkScale.get_Checked())
			{
				num6 /= 20f;
			}
			double num7 = (float)ChartWidth / num6;
			if (num7 < 500.0)
			{
				((Control)lblScale).set_Text(string.Format("{0,1:f0} masec", num7));
			}
			else
			{
				((Control)lblScale).set_Text(string.Format("{0,1:f2}\"", num7 / 1000.0));
			}
			float num8 = 0f;
			float num9 = 0f;
			if (Settings.Default.GraphicsSmoothed)
			{
				formGraphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			new Font("Arial", 7f);
			Font font = new Font("Arial", 8f);
			Font font2 = new Font("Arial", 9f, FontStyle.Bold);
			new Font("Arial", 12f, FontStyle.Bold);
			Cursor.set_Current(Cursors.get_WaitCursor());
			SetMulticolorPens();
			Pen pen;
			Brush brush;
			Brush brush2;
			Pen pen2;
			Pen pen3;
			if (@checked || Printer)
			{
				pen = new Pen(Brushes.Black, 1f);
				new Pen(Brushes.Black, 2f);
				brush = Brushes.Black;
				brush2 = Brushes.Black;
				pen2 = new Pen(Brushes.Black, 1f);
				pen3 = new Pen(Brushes.Black, 1f);
			}
			else
			{
				pen = new Pen(Brushes.WhiteSmoke, 1f);
				new Pen(Brushes.WhiteSmoke, 2f);
				brush = Brushes.White;
				brush2 = Brushes.Yellow;
				pen2 = new Pen(Brushes.Yellow, 1f);
				pen3 = new Pen(Brushes.AntiqueWhite, 1f);
			}
			Pen pen4 = new Pen(Brushes.WhiteSmoke, 1f);
			pen3.DashPattern = new float[2] { 1f, 2f };
			if (@checked || Printer)
			{
				formGraphics.Clear(Color.White);
			}
			else
			{
				formGraphics.Clear(Color.Black);
			}
			formGraphics.DrawEllipse(pen, (float)ChartWidth / 2f - num6 * num3, (float)ChartHeight / 2f - num6 * num3, 2f * num6 * num3, 2f * num6 * num3);
			formGraphics.FillEllipse(brush, (float)ChartWidth / 2f - 3f, (float)ChartHeight / 2f - 3f, 6f, 6f);
			if (!Printer)
			{
				Tags.Clear();
			}
			float x;
			float y;
			for (int i = 0; i < Offsets.Count; i++)
			{
				pen4 = PenChords[i % 30];
				num = (15.0 * Offsets[i].RAatEpoch - TargetRA) * 3600.0 * 1000.0 * Math.Cos(TargetDec / (180.0 / Math.PI));
				num2 = (Offsets[i].DECatEpoch - TargetDec) * 3600.0 * 1000.0;
				if ((Math.Abs(num) > 10000.0) | (Math.Abs(num2) > 10000.0))
				{
					continue;
				}
				num8 = (float)((double)((float)ChartWidth / 2f) - (double)num6 * num);
				num9 = (float)((double)((float)ChartWidth / 2f) - (double)num6 * num2);
				pen4.DashStyle = DashStyle.Solid;
				pen4.Width = 2f;
				formGraphics.DrawEllipse(pen4, num8 - 2f, num9 - 2f, 4f, 4f);
				if (chkStarUncertainty.get_Checked())
				{
					num4 = (float)Offsets[i].RAUncertAtEpoch * num6 * 1000f;
					num5 = (float)Offsets[i].DecUncertaintyAtEpoch * num6 * 1000f;
					pen4.DashPattern = new float[4] { 3f, 5f, 1.5f, 5f };
					if (num4 < 0f || num5 < 0f)
					{
						pen4.DashPattern = new float[2] { 2f, 15f };
					}
					pen4.Width = 0.8f;
					formGraphics.DrawEllipse(pen4, num8 - num4, num9 - num5, 2f * num4, 2f * num5);
				}
				else
				{
					pen4.Width = 2f;
					if (ShowProperMotions)
					{
						x = (float)((double)((float)ChartWidth / 2f) - (double)num6 * (num - 10.0 * Offsets[i].pmRA * 3600.0 * 15000.0 * Math.Cos(TargetDec / (180.0 / Math.PI))));
						y = (float)((double)((float)ChartWidth / 2f) - (double)num6 * (num2 - 10.0 * Offsets[i].pmDEC * 3600.0 * 1000.0));
						pen4.DashPattern = new float[2] { 1f, 3f };
						formGraphics.DrawLine(pen4, num8, num9, x, y);
						pen4.DashPattern = new float[2] { 100f, 1f };
					}
				}
				StarCatTags starCatTags = new StarCatTags();
				starCatTags.X = num8;
				starCatTags.Y = num9;
				if (stars.Count > 0)
				{
					starCatTags.Tag = stars[i].StarName;
				}
				else
				{
					starCatTags.Tag = "not set";
				}
				Tags.Add(starCatTags);
				pen4.Width = 2f;
				if ((Offsets[i].piRA != 0.0) & (Offsets[i].piDec != 0.0))
				{
					x = (float)((double)((float)ChartWidth / 2f) - (double)num6 * (num - Offsets[i].piRA * 3600.0 * 15000.0 * Math.Cos(TargetDec / (180.0 / Math.PI))));
					y = (float)((double)((float)ChartWidth / 2f) - (double)num6 * (num2 - Offsets[i].piDec * 3600.0 * 1000.0));
					pen4.DashPattern = new float[2] { 0.4f, 4f };
					formGraphics.DrawLine(pen4, num8, num9, x, y);
				}
				pen4.DashPattern = new float[2] { 100f, 1f };
			}
			Tags.Sort();
			float num11;
			float num12;
			if (chkShowMotion.get_Checked())
			{
				double num10 = (double)updnMotionPA.get_Value() / (180.0 / Math.PI);
				num8 = (float)((double)((float)ChartWidth / 2f) - (double)ChartWidth * Math.Sin(num10));
				num9 = (float)((double)((float)ChartWidth / 2f) - (double)ChartWidth * Math.Cos(num10));
				x = (float)((double)((float)ChartWidth / 2f) + (double)ChartWidth * Math.Sin(num10));
				y = (float)((double)((float)ChartWidth / 2f) + (double)ChartWidth * Math.Cos(num10));
				num11 = (float)((double)((0f - num6) * num3) * Math.Cos(num10));
				num12 = (float)((double)(num6 * num3) * Math.Sin(num10));
				formGraphics.DrawLine(pen3, num8 + num11, num9 + num12, x + num11, y + num12);
				formGraphics.DrawLine(pen3, num8 - num11, num9 - num12, x - num11, y - num12);
			}
			float num13 = (float)num7;
			float num14 = 1f;
			if (num13 > 12000f)
			{
				num13 = 10000f;
				num14 = 10f;
			}
			else if (num13 > 6000f)
			{
				num13 = 5000f;
				num14 = 5f;
			}
			else if (num13 > 5000f)
			{
				num13 = 4000f;
				num14 = 4f;
			}
			else if (num13 > 2500f)
			{
				num13 = 2000f;
				num14 = 2f;
			}
			else if (num13 > 1200f)
			{
				num13 = 1000f;
				num14 = 10f;
			}
			else if (num13 > 600f)
			{
				num13 = 500f;
				num14 = 5f;
			}
			else if (num13 > 500f)
			{
				num13 = 400f;
				num14 = 4f;
			}
			else if (num13 > 250f)
			{
				num13 = 200f;
				num14 = 2f;
			}
			else if (num13 > 120f)
			{
				num13 = 100f;
				num14 = 10f;
			}
			else if (num13 > 60f)
			{
				num13 = 50f;
				num14 = 5f;
			}
			else if (num13 > 50f)
			{
				num13 = 40f;
				num14 = 4f;
			}
			else if (num13 > 25f)
			{
				num13 = 20f;
				num14 = 2f;
			}
			else if (num13 > 12f)
			{
				num13 = 10f;
				num14 = 10f;
			}
			else if (num13 > 6f)
			{
				num13 = 5f;
				num14 = 5f;
			}
			else
			{
				num13 = 2f;
				num14 = 2f;
			}
			x = (float)ChartWidth / 2f - num6 * num13 / 2f;
			num11 = (float)ChartWidth / 2f + num6 * num13 / 2f;
			y = ChartHeight - 8;
			formGraphics.DrawLine(pen2, x, y, num11, y);
			formGraphics.DrawLine(pen2, x, y + 5f, x, y - 12f);
			formGraphics.DrawLine(pen2, num11, y + 5f, num11, y - 10f);
			float num15 = (num11 - x) / num14;
			for (int j = 1; (float)j < num14; j++)
			{
				formGraphics.DrawLine(pen2, x + (float)j * num15, y + 5f, x + (float)j * num15, y);
			}
			string text = string.Format("{0,1:F0} masec", num13);
			if (num13 > 100f)
			{
				text = string.Format("{0,1:F2}\"", num13 / 1000f);
			}
			font = new Font("Times New Roman", 8f, FontStyle.Regular);
			formGraphics.DrawString(text, font, brush, (float)ChartWidth / 2f - formGraphics.MeasureString(text, font).Width / 2f, ChartHeight - 20);
			formGraphics.DrawString("N", font2, brush2, (float)ChartWidth / 2f - 5f, 2f);
			formGraphics.DrawString("E", font2, brush2, 2f, (float)ChartHeight / 2f - 5f);
			string s = string.Format("{0,7:f2}", updnEpoch.get_Value());
			formGraphics.DrawString(s, font, brush2, 5f, 5f);
			double num16 = (double)updnMotionPA.get_Value() / (180.0 / Math.PI);
			x = (float)((double)(ChartWidth - 30) - 20.0 * Math.Sin(num16));
			y = (float)(30.0 - 20.0 * Math.Cos(num16));
			formGraphics.FillEllipse(brush2, ChartWidth - 32, 28, 4, 4);
			formGraphics.DrawLine(pen2, ChartWidth - 30, 30f, x, y);
			num11 = (float)((double)x + 10.0 * Math.Sin(num16 + 0.4));
			num12 = (float)((double)y + 10.0 * Math.Cos(num16 + 0.4));
			formGraphics.DrawLine(pen2, x, y, num11, num12);
			num11 = (float)((double)x + 10.0 * Math.Sin(num16 - 0.4));
			num12 = (float)((double)y + 10.0 * Math.Cos(num16 - 0.4));
			formGraphics.DrawLine(pen2, x, y, num11, num12);
			int num17 = (int)Math.Floor((double)Offsets.Count / 5.0);
			int height = 12 * stars.Count + 4 * num17 + 5;
			((Control)picLegend).set_Height(height);
			int width;
			((Control)picLegend).set_Width(width = 140);
			Bitmap image = new Bitmap(width, height);
			Graphics graphics = Graphics.FromImage(image);
			if (PenChords[0] == null)
			{
				SetMulticolorPens();
			}
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			int num18 = 0;
			foreach (StarEntry star in stars)
			{
				if (!(star.CatalogId == "II/241"))
				{
					string text2 = star.StarName;
					int num19 = text2.IndexOf(" ");
					if ((num19 > 0) & (num19 < text2.Length))
					{
						text2 = text2.Substring(0, num19);
					}
					graphics.DrawString(text2, font, Brushes.Black, 75f, (float)((double)(12 * num18) + 4.0 * Math.Floor((double)num18 / 5.0)));
					graphics.DrawLine(PenChords[num18 % 30], 5, 7 + num18 * 12 + 4 * (int)Math.Floor((double)num18 / 5.0), 70, 7 + num18 * 12 + 4 * (int)Math.Floor((double)num18 / 5.0));
					graphics.DrawLine(PenChords[num18 % 30], 5, 6 + num18 * 12 + 4 * (int)Math.Floor((double)num18 / 5.0), 70, 6 + num18 * 12 + 4 * (int)Math.Floor((double)num18 / 5.0));
					num18++;
				}
			}
			picLegend.set_Image((Image)image);
			graphics.Dispose();
			IsPlotting = false;
		}

		private static void SetMulticolorPens()
		{
			int num = 2;
			PenChords[0] = new Pen(Color.Aqua, num);
			PenChords[1] = new Pen(Color.Red, num);
			PenChords[2] = new Pen(Color.Orange, num);
			PenChords[3] = new Pen(Color.BlueViolet, num);
			PenChords[4] = new Pen(Color.Chartreuse, num);
			PenChords[5] = new Pen(Color.Yellow, num);
			PenChords[6] = new Pen(Color.HotPink, num);
			PenChords[7] = new Pen(Color.Teal, num);
			PenChords[8] = new Pen(Color.Coral, num);
			PenChords[9] = new Pen(Color.CornflowerBlue, num);
			PenChords[10] = new Pen(Color.Fuchsia, num);
			PenChords[11] = new Pen(Color.Goldenrod, num);
			PenChords[12] = new Pen(Color.Green, num);
			PenChords[13] = new Pen(Color.Gold, num);
			PenChords[14] = new Pen(Color.Crimson, num);
			PenChords[15] = new Pen(Color.IndianRed, num);
			PenChords[16] = new Pen(Color.Khaki, num);
			PenChords[17] = new Pen(Color.LightCoral, num);
			PenChords[18] = new Pen(Color.RoyalBlue, num);
			PenChords[19] = new Pen(Color.Purple, num);
			PenChords[20] = new Pen(Color.MediumVioletRed, num);
			PenChords[21] = new Pen(Color.MidnightBlue, num);
			PenChords[22] = new Pen(Color.Aquamarine, num);
			PenChords[23] = new Pen(Color.OrangeRed, num);
			PenChords[24] = new Pen(Color.RoyalBlue, num);
			PenChords[25] = new Pen(Color.SlateBlue, num);
			PenChords[26] = new Pen(Color.CadetBlue, num);
			PenChords[27] = new Pen(Color.Brown, num);
			PenChords[28] = new Pen(Color.YellowGreen, num);
			PenChords[29] = new Pen(Color.Olive, num);
		}

		private void trackBar1_ValueChanged(object sender, EventArgs e)
		{
			DrawOffsets();
		}

		private void copyImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picStarPlot.get_Image());
		}

		private void copyLegendToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Clipboard.SetImage(picLegend.get_Image());
		}

		private void saveImageLegendToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Output.SaveGraphic(picStarPlot.get_Image(), "Compare stars", Utilities.AppPath + "\\Asteroids");
			Output.SaveGraphic(picLegend.get_Image(), "Compare Stars-Legend", Utilities.AppPath + "\\Asteroids");
			Output.SavePredictionText(CollectEvents(), "Compare stars", Utilities.AppPath + "\\Asteroids");
		}

		private void whiteBackgroundToolStripMenuItem_Click(object sender, EventArgs e)
		{
			DrawOffsets();
		}

		private void saveFormAsAnImageToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Application.DoEvents();
			Bitmap bitmap = new Bitmap(((Control)this).get_Bounds().Width, ((Control)this).get_Bounds().Height);
			Graphics.FromImage(bitmap).CopyFromScreen(((Form)this).get_Location().X, ((Form)this).get_Location().Y, 0, 0, bitmap.Size);
			string text2;
			if (FromAsteroidPrediction)
			{
				string text = ((Control)lblEventDetails).get_Text();
				int num = text.IndexOf(":");
				int num2 = text.IndexOf("\r");
				text2 = text.Substring(num + 1, num2 - num - 1).Trim();
				num = text.IndexOf(":", num2);
				num2 = text.IndexOf("\r", num);
				text2 = text2 + "_" + text.Substring(num + 1, num2 - num - 1).Replace(" ", "");
				num = text.IndexOf(":", num2);
				text2 = text2 + "_" + text.Substring(num + 1, text.Length - num - 1).Replace(" ", "");
			}
			else
			{
				text2 = "StarCatalogueComparison";
			}
			Output.SaveGraphic(bitmap, text2, Utilities.AppPath + "\\Predictions");
		}

		private void chklstCats_Click(object sender, EventArgs e)
		{
			((Control)cmdReDisplay).set_Enabled(false);
		}

		private void chklstCats_KeyDown(object sender, KeyEventArgs e)
		{
			((Control)cmdReDisplay).set_Enabled(false);
		}

		private void updnRadius_ValueChanged(object sender, EventArgs e)
		{
			((Control)cmdReDisplay).set_Enabled(false);
		}

		private void cmdNow_Click(object sender, EventArgs e)
		{
			updnEpoch.set_Value((decimal)Utilities.BesselianYear(Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day)));
		}

		private void setAnimatedGIFParametersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0006: Unknown result type (might be due to invalid IL or missing references)
			GIFsettings gIFsettings = new GIFsettings();
			((Form)gIFsettings).ShowDialog();
			((Component)(object)gIFsettings).Dispose();
		}

		private void create10To5YearsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			List<string> list = new List<string>();
			int num = 0;
			ShowProperMotions = false;
			string path = Utilities.AppPath + "\\Asteroids\\AnimatedGIF";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			string[] files = Directory.GetFiles(path, "*.gif");
			foreach (string path2 in files)
			{
				try
				{
					File.Delete(path2);
				}
				catch
				{
				}
			}
			decimal value = updnEpoch.get_Value();
			for (decimal num2 = -10m; num2 <= 5m; num2 += 0.5m)
			{
				updnEpoch.set_Value(value + num2);
				string text = Utilities.AppPath + "\\Asteroids\\AnimatedGIF\\" + num + ".gif";
				FindInSpecificCatalogues(DownLoadVizier: false);
				try
				{
					picStarPlot.get_Image().Save(text, ImageFormat.Gif);
					list.Add(text);
				}
				catch
				{
				}
				num++;
			}
			ShowProperMotions = true;
			updnEpoch.set_Value(value);
			FindInSpecificCatalogues(DownLoadVizier: false);
			global::GifCreator.GifCreator.Create_Animated_Gif(list, "Asteroid.gif");
		}

		private void franceToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(0);
		}

		private void uSAToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(1);
		}

		private void canadaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(2);
		}

		private void japanToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(3);
		}

		private void indiaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(4);
		}

		private void chinaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(5);
		}

		private void uKToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(6);
		}

		private void hawaiiToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(7);
		}

		private void russiaToolStripMenuItem_Click(object sender, EventArgs e)
		{
			setVizierChecks(8);
		}

		internal void setVizierChecks(int V)
		{
			Settings.Default.VizierServer = V;
			franceToolStripMenuItem.set_Checked(V == 0);
			uSAToolStripMenuItem.set_Checked(V == 1);
			canadaToolStripMenuItem.set_Checked(V == 2);
			japanToolStripMenuItem.set_Checked(V == 3);
			indiaToolStripMenuItem.set_Checked(V == 4);
			chinaToolStripMenuItem.set_Checked(V == 5);
			uKToolStripMenuItem.set_Checked(V == 6);
			hawaiiToolStripMenuItem.set_Checked(V == 7);
			russiaToolStripMenuItem.set_Checked(V == 8);
			switch (V)
			{
			case 1:
				Vizier = VizierMirror.USA;
				break;
			case 2:
				Vizier = VizierMirror.Canada;
				break;
			case 3:
				Vizier = VizierMirror.Japan;
				break;
			case 4:
				Vizier = VizierMirror.India;
				break;
			case 5:
				Vizier = VizierMirror.China;
				break;
			case 6:
				Vizier = VizierMirror.UK;
				break;
			case 7:
				Vizier = VizierMirror.Hawaii;
				break;
			case 8:
				Vizier = VizierMirror.Russia;
				break;
			default:
				Vizier = VizierMirror.France;
				break;
			}
			((Control)cmdListPositions).set_Text("Get from \n" + Vizier.ToString() + "\n&& display");
		}

		private void chkScale_CheckedChanged(object sender, EventArgs e)
		{
			FindInSpecificCatalogues(DownLoadVizier: false);
		}

		private void lstPositions_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			//IL_001f: Invalid comparison between Unknown and I4
			if (lstPositions.get_Items().get_Count() >= 1 && (int)e.get_Button() == 2097152)
			{
				int num = lstPositions.IndexFromPoint(e.get_X(), e.get_Y());
				if (num >= 0 && num < lstPositions.get_Items().get_Count())
				{
					((ListControl)lstPositions).set_SelectedIndex(num);
				}
				((Control)lstPositions).Refresh();
			}
		}

		private void showStarInVizieRToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((ListControl)lstPositions).get_SelectedIndex() >= 0 && ((((ListControl)lstPositions).get_SelectedIndex() > 2) & (((ListControl)lstPositions).get_SelectedIndex() < HTML.Count + 3)))
			{
				((Control)this).set_Cursor(Cursors.get_WaitCursor());
				Process.Start(HTML[((ListControl)lstPositions).get_SelectedIndex() - 3]!.ToString());
				((Control)this).set_Cursor(Cursors.get_Default());
			}
		}

		private void cmdGoogleSky_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This functionality requires internet access. You are not connected to the internet", "No internet");
				return;
			}
			SetTargetLocation();
			GoogleEarth.CreateGoogleSkyKMLFile(TargetRA, TargetDec, 5, Utilities.AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void cmdDoubles_Click(object sender, EventArgs e)
		{
			Interferometric_Plus_WDS.Find_WDS_IF_Matches(TargetRA / 15.0, TargetDec, HighPrecision: true, ShowResults: true, out WDS_Interferometer_Display.VariableID);
		}

		private void picStarPlot_MouseMove(object sender, MouseEventArgs e)
		{
			if (GettingTag)
			{
				return;
			}
			float num = e.get_X();
			float num2 = e.get_Y();
			if (Tags.Count < 1 || ((Math.Abs(num - OldX) < 2f) & (Math.Abs(num2 - OldY) < 2f)))
			{
				return;
			}
			GettingTag = true;
			string text = "";
			for (int i = 0; i < Tags.Count; i++)
			{
				if ((Math.Abs(num - Tags[i].X) < 3f) & (Math.Abs(num2 - Tags[i].Y) < 3f))
				{
					text = ((text.Length != 0) ? (text + "\r\n" + Tags[i].Tag) : Tags[i].Tag);
					OldX = num;
					OldY = num2;
				}
			}
			GetToolTip = !GetToolTip;
			if (GetToolTip)
			{
				toolTip.SetToolTip((Control)(object)picStarPlot, text);
			}
			GettingTag = false;
		}

		private void chkParallax_CheckedChanged(object sender, EventArgs e)
		{
			FindInSpecificCatalogues(DownLoadVizier: false);
		}

		private void chkStarUncertainty_CheckedChanged(object sender, EventArgs e)
		{
			DrawOffsets();
		}

		private void chkShowMotion_CheckedChanged(object sender, EventArgs e)
		{
			DrawOffsets();
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
			//IL_01be: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c8: Expected O, but got Unknown
			//IL_01c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d3: Expected O, but got Unknown
			//IL_01d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_01de: Expected O, but got Unknown
			//IL_01df: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e9: Expected O, but got Unknown
			//IL_01ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f4: Expected O, but got Unknown
			//IL_01f5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01ff: Expected O, but got Unknown
			//IL_0200: Unknown result type (might be due to invalid IL or missing references)
			//IL_020a: Expected O, but got Unknown
			//IL_020b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0215: Expected O, but got Unknown
			//IL_0216: Unknown result type (might be due to invalid IL or missing references)
			//IL_0220: Expected O, but got Unknown
			//IL_0221: Unknown result type (might be due to invalid IL or missing references)
			//IL_022b: Expected O, but got Unknown
			//IL_022c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0236: Expected O, but got Unknown
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0241: Expected O, but got Unknown
			//IL_0242: Unknown result type (might be due to invalid IL or missing references)
			//IL_024c: Expected O, but got Unknown
			//IL_024d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0257: Expected O, but got Unknown
			//IL_0258: Unknown result type (might be due to invalid IL or missing references)
			//IL_0262: Expected O, but got Unknown
			//IL_0263: Unknown result type (might be due to invalid IL or missing references)
			//IL_026d: Expected O, but got Unknown
			//IL_026e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0278: Expected O, but got Unknown
			//IL_0279: Unknown result type (might be due to invalid IL or missing references)
			//IL_0283: Expected O, but got Unknown
			//IL_0284: Unknown result type (might be due to invalid IL or missing references)
			//IL_028e: Expected O, but got Unknown
			//IL_028f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0299: Expected O, but got Unknown
			//IL_029a: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a4: Expected O, but got Unknown
			//IL_02a5: Unknown result type (might be due to invalid IL or missing references)
			//IL_02af: Expected O, but got Unknown
			//IL_02b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ba: Expected O, but got Unknown
			//IL_02bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c5: Expected O, but got Unknown
			//IL_02c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d0: Expected O, but got Unknown
			//IL_02d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02db: Expected O, but got Unknown
			//IL_02dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e6: Expected O, but got Unknown
			//IL_02e7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f1: Expected O, but got Unknown
			//IL_02f2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02fc: Expected O, but got Unknown
			//IL_02fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0307: Expected O, but got Unknown
			//IL_0308: Unknown result type (might be due to invalid IL or missing references)
			//IL_0312: Expected O, but got Unknown
			//IL_0313: Unknown result type (might be due to invalid IL or missing references)
			//IL_031d: Expected O, but got Unknown
			//IL_031e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0328: Expected O, but got Unknown
			//IL_0329: Unknown result type (might be due to invalid IL or missing references)
			//IL_0333: Expected O, but got Unknown
			//IL_0334: Unknown result type (might be due to invalid IL or missing references)
			//IL_033e: Expected O, but got Unknown
			//IL_033f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0349: Expected O, but got Unknown
			//IL_034a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0354: Expected O, but got Unknown
			//IL_0355: Unknown result type (might be due to invalid IL or missing references)
			//IL_035f: Expected O, but got Unknown
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
			//IL_045e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0468: Expected O, but got Unknown
			//IL_0469: Unknown result type (might be due to invalid IL or missing references)
			//IL_0473: Expected O, but got Unknown
			//IL_0474: Unknown result type (might be due to invalid IL or missing references)
			//IL_047e: Expected O, but got Unknown
			//IL_047f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0489: Expected O, but got Unknown
			//IL_048a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0494: Expected O, but got Unknown
			//IL_29d7: Unknown result type (might be due to invalid IL or missing references)
			//IL_29e1: Expected O, but got Unknown
			//IL_2b2e: Unknown result type (might be due to invalid IL or missing references)
			//IL_2b38: Expected O, but got Unknown
			//IL_37fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_3806: Expected O, but got Unknown
			//IL_385c: Unknown result type (might be due to invalid IL or missing references)
			//IL_3866: Expected O, but got Unknown
			//IL_3bfb: Unknown result type (might be due to invalid IL or missing references)
			//IL_3c05: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(CompareStarCats));
			grpStar = new GroupBox();
			lblEventDetails = new Label();
			panelCoordinates = new Panel();
			label29 = new Label();
			label11 = new Label();
			label14 = new Label();
			txtm = new TextBox();
			txtMp = new TextBox();
			txtMv = new TextBox();
			txtD = new TextBox();
			label5 = new Label();
			txtRAs = new TextBox();
			txts = new TextBox();
			txtRAm = new TextBox();
			txtRAh = new TextBox();
			label16 = new Label();
			label13 = new Label();
			label12 = new Label();
			panelSelectCatalog = new Panel();
			panelUCAC4 = new Panel();
			txtUCNumber = new TextBox();
			txtUCZone = new TextBox();
			label27 = new Label();
			label28 = new Label();
			panelTycho2 = new Panel();
			txtTycComp = new TextBox();
			txtTycSeqNum = new TextBox();
			txtTycRegion = new TextBox();
			label8 = new Label();
			label7 = new Label();
			label19 = new Label();
			label6 = new Label();
			panelHip = new Panel();
			txtHip = new TextBox();
			cmdGetStarPos = new Button();
			label10 = new Label();
			panelB1 = new Panel();
			label9 = new Label();
			txtB1number = new TextBox();
			txtB1zone = new TextBox();
			cmbCatalogue = new ComboBox();
			label4 = new Label();
			label1 = new Label();
			label2 = new Label();
			label3 = new Label();
			cmdListPositions = new Button();
			updnRadius = new NumericUpDown();
			label15 = new Label();
			menuStrip1 = new MenuStrip();
			vizierSiteToolStripMenuItem = new ToolStripMenuItem();
			franceToolStripMenuItem = new ToolStripMenuItem();
			uSAToolStripMenuItem = new ToolStripMenuItem();
			canadaToolStripMenuItem = new ToolStripMenuItem();
			japanToolStripMenuItem = new ToolStripMenuItem();
			indiaToolStripMenuItem = new ToolStripMenuItem();
			chinaToolStripMenuItem = new ToolStripMenuItem();
			uKToolStripMenuItem = new ToolStripMenuItem();
			hawaiiToolStripMenuItem = new ToolStripMenuItem();
			russiaToolStripMenuItem = new ToolStripMenuItem();
			withListToolStripMenuItem = new ToolStripMenuItem();
			whiteBackgroundToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator1 = new ToolStripSeparator();
			copyToolStripMenuItem = new ToolStripMenuItem();
			copyImageToolStripMenuItem = new ToolStripMenuItem();
			copyLegendToolStripMenuItem = new ToolStripMenuItem();
			saveImageLegendToolStripMenuItem = new ToolStripMenuItem();
			toolStripSeparator2 = new ToolStripSeparator();
			saveFormAsAnImageToolStripMenuItem = new ToolStripMenuItem();
			animatedGifToolStripMenuItem = new ToolStripMenuItem();
			setAnimatedGIFParametersToolStripMenuItem = new ToolStripMenuItem();
			create10To5YearsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			chklstCats = new CheckedListBox();
			label18 = new Label();
			lstPositions = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			showStarInVizieRToolStripMenuItem = new ToolStripMenuItem();
			updnEpoch = new NumericUpDown();
			label17 = new Label();
			label20 = new Label();
			updnDiameter = new NumericUpDown();
			trackBar1 = new TrackBar();
			lblScale = new Label();
			label23 = new Label();
			label21 = new Label();
			updnMotionPA = new NumericUpDown();
			chkShowMotion = new CheckBox();
			label22 = new Label();
			label24 = new Label();
			cmdReDisplay = new Button();
			cmdNow = new Button();
			chkScale = new CheckBox();
			label25 = new Label();
			txtDuration = new TextBox();
			cmdGoogleSky = new Button();
			cmdDoubles = new Button();
			label26 = new Label();
			toolTip = new ToolTip(components);
			picLegend = new PictureBox();
			picStarPlot = new PictureBox();
			chkStarUncertainty = new CheckBox();
			chkParallax = new CheckBox();
			((Control)grpStar).SuspendLayout();
			((Control)panelCoordinates).SuspendLayout();
			((Control)panelSelectCatalog).SuspendLayout();
			((Control)panelUCAC4).SuspendLayout();
			((Control)panelTycho2).SuspendLayout();
			((Control)panelHip).SuspendLayout();
			((Control)panelB1).SuspendLayout();
			((ISupportInitialize)updnRadius).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)contextMenuStrip1).SuspendLayout();
			((ISupportInitialize)updnEpoch).BeginInit();
			((ISupportInitialize)updnDiameter).BeginInit();
			((ISupportInitialize)trackBar1).BeginInit();
			((ISupportInitialize)updnMotionPA).BeginInit();
			((ISupportInitialize)picLegend).BeginInit();
			((ISupportInitialize)picStarPlot).BeginInit();
			((Control)this).SuspendLayout();
			((Control)grpStar).get_Controls().Add((Control)(object)lblEventDetails);
			((Control)grpStar).get_Controls().Add((Control)(object)panelCoordinates);
			((Control)grpStar).get_Controls().Add((Control)(object)panelSelectCatalog);
			((Control)grpStar).get_Controls().Add((Control)(object)label4);
			((Control)grpStar).get_Controls().Add((Control)(object)label1);
			((Control)grpStar).get_Controls().Add((Control)(object)label2);
			((Control)grpStar).get_Controls().Add((Control)(object)label3);
			((Control)grpStar).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpStar).set_Location(new Point(12, 23));
			((Control)grpStar).set_Name("grpStar");
			((Control)grpStar).set_Size(new Size(477, 140));
			((Control)grpStar).set_TabIndex(0);
			grpStar.set_TabStop(false);
			((Control)grpStar).set_Text("Star");
			((Control)lblEventDetails).set_AutoSize(true);
			((Control)lblEventDetails).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblEventDetails).set_Location(new Point(16, 36));
			((Control)lblEventDetails).set_Name("lblEventDetails");
			((Control)lblEventDetails).set_Size(new Size(50, 13));
			((Control)lblEventDetails).set_TabIndex(26);
			((Control)lblEventDetails).set_Text("Number");
			((Control)lblEventDetails).set_Visible(false);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)label29);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)label11);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)label14);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txtm);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txtMp);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txtMv);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txtD);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)label5);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txtRAs);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txts);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txtRAm);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)txtRAh);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)label16);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)label13);
			((Control)panelCoordinates).get_Controls().Add((Control)(object)label12);
			((Control)panelCoordinates).set_Location(new Point(322, 23));
			((Control)panelCoordinates).set_Name("panelCoordinates");
			((Control)panelCoordinates).set_Size(new Size(149, 105));
			((Control)panelCoordinates).set_TabIndex(25);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(3, 48));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(27, 13));
			((Control)label29).set_TabIndex(24);
			((Control)label29).set_Text("Dec");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(5, 87));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(22, 13));
			((Control)label11).set_TabIndex(22);
			((Control)label11).set_Text("Mp");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(77, 87));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(24, 13));
			((Control)label14).set_TabIndex(20);
			((Control)label14).set_Text("Mv");
			((Control)txtm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtm).set_Location(new Point(63, 45));
			((Control)txtm).set_Name("txtm");
			((Control)txtm).set_Size(new Size(25, 20));
			((Control)txtm).set_TabIndex(17);
			((Control)txtm).set_Text("0");
			((Control)txtMp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMp).set_Location(new Point(33, 83));
			((Control)txtMp).set_Name("txtMp");
			((TextBoxBase)txtMp).set_ReadOnly(true);
			((Control)txtMp).set_Size(new Size(37, 20));
			((Control)txtMp).set_TabIndex(23);
			((Control)txtMp).set_Text("12.0");
			((Control)txtMv).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)txtMv).set_Location(new Point(102, 83));
			((Control)txtMv).set_Name("txtMv");
			((TextBoxBase)txtMv).set_ReadOnly(true);
			((Control)txtMv).set_Size(new Size(39, 20));
			((Control)txtMv).set_TabIndex(21);
			((Control)txtMv).set_Text("12.0");
			((Control)txtD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtD).set_ImeMode((ImeMode)0);
			((Control)txtD).set_Location(new Point(31, 45));
			((Control)txtD).set_Name("txtD");
			((Control)txtD).set_Size(new Size(26, 20));
			((Control)txtD).set_TabIndex(15);
			((Control)txtD).set_Text("0");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(3, 17));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(22, 13));
			((Control)label5).set_TabIndex(6);
			((Control)label5).set_Text("RA");
			((Control)txtRAs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAs).set_Location(new Point(94, 13));
			((Control)txtRAs).set_Name("txtRAs");
			((Control)txtRAs).set_Size(new Size(48, 20));
			((Control)txtRAs).set_TabIndex(12);
			((Control)txtRAs).set_Text("0.0");
			((Control)txts).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txts).set_Location(new Point(94, 45));
			((Control)txts).set_Name("txts");
			((Control)txts).set_Size(new Size(41, 20));
			((Control)txts).set_TabIndex(19);
			((Control)txts).set_Text("0.0");
			((Control)txtRAm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAm).set_Location(new Point(63, 13));
			((Control)txtRAm).set_Name("txtRAm");
			((Control)txtRAm).set_Size(new Size(25, 20));
			((Control)txtRAm).set_TabIndex(10);
			((Control)txtRAm).set_Text("0");
			((Control)txtRAh).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRAh).set_Location(new Point(31, 13));
			((Control)txtRAh).set_Name("txtRAh");
			((Control)txtRAh).set_Size(new Size(26, 20));
			((Control)txtRAh).set_TabIndex(8);
			((Control)txtRAh).set_Text("0");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(103, 38));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(12, 13));
			((Control)label16).set_TabIndex(18);
			((Control)label16).set_Text("\"");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(68, 38));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(9, 13));
			((Control)label13).set_TabIndex(16);
			((Control)label13).set_Text("'");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(39, 33));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(13, 13));
			((Control)label12).set_TabIndex(14);
			((Control)label12).set_Text("o");
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)panelUCAC4);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)panelTycho2);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)label19);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)label6);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)panelHip);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)cmdGetStarPos);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)label10);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)panelB1);
			((Control)panelSelectCatalog).get_Controls().Add((Control)(object)cmbCatalogue);
			((Control)panelSelectCatalog).set_Location(new Point(15, 23));
			((Control)panelSelectCatalog).set_Name("panelSelectCatalog");
			((Control)panelSelectCatalog).set_Size(new Size(299, 108));
			((Control)panelSelectCatalog).set_TabIndex(24);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtUCNumber);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)txtUCZone);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)label27);
			((Control)panelUCAC4).get_Controls().Add((Control)(object)label28);
			((Control)panelUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelUCAC4).set_Location(new Point(65, 32));
			((Control)panelUCAC4).set_Name("panelUCAC4");
			((Control)panelUCAC4).set_Size(new Size(142, 33));
			((Control)panelUCAC4).set_TabIndex(6);
			((Control)txtUCNumber).set_Location(new Point(71, 7));
			((Control)txtUCNumber).set_Name("txtUCNumber");
			((Control)txtUCNumber).set_Size(new Size(57, 20));
			((Control)txtUCNumber).set_TabIndex(1);
			((Control)txtUCZone).set_Location(new Point(21, 7));
			((Control)txtUCZone).set_Name("txtUCZone");
			((Control)txtUCZone).set_Size(new Size(39, 20));
			((Control)txtUCZone).set_TabIndex(0);
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(101, 10));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(11, 13));
			((Control)label27).set_TabIndex(4);
			((Control)label27).set_Text("-");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(46, 10));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(11, 13));
			((Control)label28).set_TabIndex(3);
			((Control)label28).set_Text("-");
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycComp);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycSeqNum);
			((Control)panelTycho2).get_Controls().Add((Control)(object)txtTycRegion);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label8);
			((Control)panelTycho2).get_Controls().Add((Control)(object)label7);
			((Control)panelTycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelTycho2).set_Location(new Point(66, 32));
			((Control)panelTycho2).set_Name("panelTycho2");
			((Control)panelTycho2).set_Size(new Size(142, 33));
			((Control)panelTycho2).set_TabIndex(3);
			((Control)txtTycComp).set_Location(new Point(111, 7));
			((Control)txtTycComp).set_Name("txtTycComp");
			((Control)txtTycComp).set_Size(new Size(17, 20));
			((Control)txtTycComp).set_TabIndex(2);
			((Control)txtTycComp).set_Text("1");
			((Control)txtTycSeqNum).set_Location(new Point(56, 7));
			((Control)txtTycSeqNum).set_Name("txtTycSeqNum");
			((Control)txtTycSeqNum).set_Size(new Size(47, 20));
			((Control)txtTycSeqNum).set_TabIndex(1);
			((Control)txtTycRegion).set_Location(new Point(8, 7));
			((Control)txtTycRegion).set_Name("txtTycRegion");
			((Control)txtTycRegion).set_Size(new Size(39, 20));
			((Control)txtTycRegion).set_TabIndex(0);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(101, 10));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(11, 13));
			((Control)label8).set_TabIndex(4);
			((Control)label8).set_Text("-");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(46, 10));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(11, 13));
			((Control)label7).set_TabIndex(3);
			((Control)label7).set_Text("-");
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(220, 59));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(77, 42));
			((Control)label19).set_TabIndex(5);
			((Control)label19).set_Text("- or manually \r\nenter the star\r\n coordinates");
			label19.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(3, 12));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(64, 13));
			((Control)label6).set_TabIndex(0);
			((Control)label6).set_Text("Catalogue");
			((Control)panelHip).get_Controls().Add((Control)(object)txtHip);
			((Control)panelHip).set_Location(new Point(66, 32));
			((Control)panelHip).set_Name("panelHip");
			((Control)panelHip).set_Size(new Size(141, 33));
			((Control)panelHip).set_TabIndex(3);
			((Control)txtHip).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtHip).set_Location(new Point(25, 7));
			((Control)txtHip).set_Name("txtHip");
			((Control)txtHip).set_Size(new Size(70, 20));
			((Control)txtHip).set_TabIndex(2);
			((Control)cmdGetStarPos).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGetStarPos).set_Location(new Point(223, 8));
			((Control)cmdGetStarPos).set_Name("cmdGetStarPos");
			((Control)cmdGetStarPos).set_Size(new Size(69, 34));
			((Control)cmdGetStarPos).set_TabIndex(4);
			((Control)cmdGetStarPos).set_Text("Get\r\ncoords");
			((ButtonBase)cmdGetStarPos).set_UseVisualStyleBackColor(true);
			((Control)cmdGetStarPos).add_Click((EventHandler)cmdGetStarPos_Click);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(13, 42));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(50, 13));
			((Control)label10).set_TabIndex(1);
			((Control)label10).set_Text("Number");
			((Control)panelB1).get_Controls().Add((Control)(object)label9);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1number);
			((Control)panelB1).get_Controls().Add((Control)(object)txtB1zone);
			((Control)panelB1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panelB1).set_Location(new Point(66, 32));
			((Control)panelB1).set_Name("panelB1");
			((Control)panelB1).set_Size(new Size(141, 33));
			((Control)panelB1).set_TabIndex(4);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(48, 10));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(11, 13));
			((Control)label9).set_TabIndex(2);
			((Control)label9).set_Text("-");
			((Control)txtB1number).set_Location(new Point(60, 7));
			((Control)txtB1number).set_Name("txtB1number");
			((Control)txtB1number).set_Size(new Size(70, 20));
			((Control)txtB1number).set_TabIndex(0);
			((Control)txtB1zone).set_Location(new Point(8, 7));
			((Control)txtB1zone).set_Name("txtB1zone");
			((Control)txtB1zone).set_Size(new Size(39, 20));
			((Control)txtB1zone).set_TabIndex(0);
			cmbCatalogue.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbCatalogue).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ListControl)cmbCatalogue).set_FormattingEnabled(true);
			cmbCatalogue.get_Items().AddRange(new object[4] { "Hipparcos", "Tycho-2", "UCAC4", "USNO-B1" });
			((Control)cmbCatalogue).set_Location(new Point(91, 9));
			((Control)cmbCatalogue).set_Name("cmbCatalogue");
			((Control)cmbCatalogue).set_Size(new Size(96, 21));
			((Control)cmbCatalogue).set_TabIndex(2);
			cmbCatalogue.add_SelectedIndexChanged((EventHandler)cmbCatalogue_SelectedIndexChanged);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(320, 72));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(27, 13));
			((Control)label4).set_TabIndex(13);
			((Control)label4).set_Text("Dec");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(432, 22));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(12, 13));
			((Control)label1).set_TabIndex(11);
			((Control)label1).set_Text("s");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(390, 22));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(15, 13));
			((Control)label2).set_TabIndex(9);
			((Control)label2).set_Text("m");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(361, 22));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(13, 13));
			((Control)label3).set_TabIndex(7);
			((Control)label3).set_Text("h");
			((Control)cmdListPositions).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdListPositions).set_Location(new Point(267, 311));
			((Control)cmdListPositions).set_Name("cmdListPositions");
			((Control)cmdListPositions).set_Size(new Size(104, 47));
			((Control)cmdListPositions).set_TabIndex(15);
			((Control)cmdListPositions).set_Text("Get && show positions");
			((ButtonBase)cmdListPositions).set_UseVisualStyleBackColor(true);
			((Control)cmdListPositions).add_Click((EventHandler)cmdListPositions_Click);
			((Control)updnRadius).set_Location(new Point(422, 168));
			updnRadius.set_Maximum(new decimal(new int[4] { 200, 0, 0, 0 }));
			updnRadius.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnRadius).set_Name("updnRadius");
			((Control)updnRadius).set_Size(new Size(41, 20));
			((Control)updnRadius).set_TabIndex(4);
			((UpDownBase)updnRadius).set_TextAlign((HorizontalAlignment)2);
			updnRadius.set_Value(new decimal(new int[4] { 4, 0, 0, 0 }));
			updnRadius.add_ValueChanged((EventHandler)updnRadius_ValueChanged);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(302, 172));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(118, 13));
			((Control)label15).set_TabIndex(3);
			((Control)label15).set_Text("Search Radius (arcsec)");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[5]
			{
				(ToolStripItem)vizierSiteToolStripMenuItem,
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)animatedGifToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1061, 24));
			((Control)menuStrip1).set_TabIndex(25);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)vizierSiteToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[9]
			{
				(ToolStripItem)franceToolStripMenuItem,
				(ToolStripItem)uSAToolStripMenuItem,
				(ToolStripItem)canadaToolStripMenuItem,
				(ToolStripItem)japanToolStripMenuItem,
				(ToolStripItem)indiaToolStripMenuItem,
				(ToolStripItem)chinaToolStripMenuItem,
				(ToolStripItem)uKToolStripMenuItem,
				(ToolStripItem)hawaiiToolStripMenuItem,
				(ToolStripItem)russiaToolStripMenuItem
			});
			((ToolStripItem)vizierSiteToolStripMenuItem).set_Name("vizierSiteToolStripMenuItem");
			((ToolStripItem)vizierSiteToolStripMenuItem).set_Size(new Size(77, 20));
			((ToolStripItem)vizierSiteToolStripMenuItem).set_Text("Vizier site...");
			franceToolStripMenuItem.set_Checked(true);
			franceToolStripMenuItem.set_CheckState((CheckState)1);
			((ToolStripItem)franceToolStripMenuItem).set_Image((Image)Resources.FLGFRAN);
			((ToolStripItem)franceToolStripMenuItem).set_Name("franceToolStripMenuItem");
			((ToolStripItem)franceToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)franceToolStripMenuItem).set_Text("France");
			((ToolStripItem)franceToolStripMenuItem).add_Click((EventHandler)franceToolStripMenuItem_Click);
			((ToolStripItem)uSAToolStripMenuItem).set_Image((Image)Resources.FLGUSA02);
			((ToolStripItem)uSAToolStripMenuItem).set_Name("uSAToolStripMenuItem");
			((ToolStripItem)uSAToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)uSAToolStripMenuItem).set_Text("USA");
			((ToolStripItem)uSAToolStripMenuItem).add_Click((EventHandler)uSAToolStripMenuItem_Click);
			((ToolStripItem)canadaToolStripMenuItem).set_Image((Image)Resources.FLGCAN);
			((ToolStripItem)canadaToolStripMenuItem).set_Name("canadaToolStripMenuItem");
			((ToolStripItem)canadaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)canadaToolStripMenuItem).set_Text("Canada");
			((ToolStripItem)canadaToolStripMenuItem).add_Click((EventHandler)canadaToolStripMenuItem_Click);
			((ToolStripItem)japanToolStripMenuItem).set_Image((Image)Resources.FLGJAPAN);
			((ToolStripItem)japanToolStripMenuItem).set_Name("japanToolStripMenuItem");
			((ToolStripItem)japanToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)japanToolStripMenuItem).set_Text("Japan");
			((ToolStripItem)japanToolStripMenuItem).add_Click((EventHandler)japanToolStripMenuItem_Click);
			((ToolStripItem)indiaToolStripMenuItem).set_Image((Image)Resources.FLGIndia);
			((ToolStripItem)indiaToolStripMenuItem).set_Name("indiaToolStripMenuItem");
			((ToolStripItem)indiaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)indiaToolStripMenuItem).set_Text("India");
			((ToolStripItem)indiaToolStripMenuItem).add_Click((EventHandler)indiaToolStripMenuItem_Click);
			((ToolStripItem)chinaToolStripMenuItem).set_Image((Image)Resources.FLGChina);
			((ToolStripItem)chinaToolStripMenuItem).set_Name("chinaToolStripMenuItem");
			((ToolStripItem)chinaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)chinaToolStripMenuItem).set_Text("China");
			((ToolStripItem)chinaToolStripMenuItem).add_Click((EventHandler)chinaToolStripMenuItem_Click);
			((ToolStripItem)uKToolStripMenuItem).set_Image((Image)Resources.FLGUK);
			((ToolStripItem)uKToolStripMenuItem).set_Name("uKToolStripMenuItem");
			((ToolStripItem)uKToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)uKToolStripMenuItem).set_Text("UK");
			((ToolStripItem)uKToolStripMenuItem).add_Click((EventHandler)uKToolStripMenuItem_Click);
			((ToolStripItem)hawaiiToolStripMenuItem).set_Image((Image)Resources.FLGUSA01);
			((ToolStripItem)hawaiiToolStripMenuItem).set_Name("hawaiiToolStripMenuItem");
			((ToolStripItem)hawaiiToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)hawaiiToolStripMenuItem).set_Text("Hawaii");
			((ToolStripItem)hawaiiToolStripMenuItem).add_Click((EventHandler)hawaiiToolStripMenuItem_Click);
			((ToolStripItem)russiaToolStripMenuItem).set_Image((Image)Resources.FLGRUS);
			((ToolStripItem)russiaToolStripMenuItem).set_Name("russiaToolStripMenuItem");
			((ToolStripItem)russiaToolStripMenuItem).set_Size(new Size(114, 22));
			((ToolStripItem)russiaToolStripMenuItem).set_Text("Russia");
			((ToolStripItem)russiaToolStripMenuItem).add_Click((EventHandler)russiaToolStripMenuItem_Click);
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[8]
			{
				(ToolStripItem)whiteBackgroundToolStripMenuItem,
				(ToolStripItem)toolStripSeparator1,
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)copyImageToolStripMenuItem,
				(ToolStripItem)copyLegendToolStripMenuItem,
				(ToolStripItem)saveImageLegendToolStripMenuItem,
				(ToolStripItem)toolStripSeparator2,
				(ToolStripItem)saveFormAsAnImageToolStripMenuItem
			});
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(96, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List....       ");
			whiteBackgroundToolStripMenuItem.set_CheckOnClick(true);
			((ToolStripItem)whiteBackgroundToolStripMenuItem).set_Image((Image)Resources.ColorHS);
			((ToolStripItem)whiteBackgroundToolStripMenuItem).set_Name("whiteBackgroundToolStripMenuItem");
			((ToolStripItem)whiteBackgroundToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)whiteBackgroundToolStripMenuItem).set_Text("White background for plot");
			((ToolStripItem)whiteBackgroundToolStripMenuItem).add_Click((EventHandler)whiteBackgroundToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator1).set_Name("toolStripSeparator1");
			((ToolStripItem)toolStripSeparator1).set_Size(new Size(240, 6));
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy position list");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)copyImageToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyImageToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyImageToolStripMenuItem).set_Name("copyImageToolStripMenuItem");
			((ToolStripItem)copyImageToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)copyImageToolStripMenuItem).set_Text("Copy image");
			((ToolStripItem)copyImageToolStripMenuItem).add_Click((EventHandler)copyImageToolStripMenuItem_Click);
			((ToolStripItem)copyLegendToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyLegendToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyLegendToolStripMenuItem).set_Name("copyLegendToolStripMenuItem");
			((ToolStripItem)copyLegendToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)copyLegendToolStripMenuItem).set_Text("Copy Legend");
			((ToolStripItem)copyLegendToolStripMenuItem).add_Click((EventHandler)copyLegendToolStripMenuItem_Click);
			((ToolStripItem)saveImageLegendToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveImageLegendToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveImageLegendToolStripMenuItem).set_Name("saveImageLegendToolStripMenuItem");
			((ToolStripItem)saveImageLegendToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)saveImageLegendToolStripMenuItem).set_Text("Save Positions, Image && Legend");
			((ToolStripItem)saveImageLegendToolStripMenuItem).add_Click((EventHandler)saveImageLegendToolStripMenuItem_Click);
			((ToolStripItem)toolStripSeparator2).set_Name("toolStripSeparator2");
			((ToolStripItem)toolStripSeparator2).set_Size(new Size(240, 6));
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Name("saveFormAsAnImageToolStripMenuItem");
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Size(new Size(243, 22));
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).set_Text("Save entire form as an image");
			((ToolStripItem)saveFormAsAnImageToolStripMenuItem).add_Click((EventHandler)saveFormAsAnImageToolStripMenuItem_Click);
			((ToolStripDropDownItem)animatedGifToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)setAnimatedGIFParametersToolStripMenuItem,
				(ToolStripItem)create10To5YearsToolStripMenuItem
			});
			((ToolStripItem)animatedGifToolStripMenuItem).set_Name("animatedGifToolStripMenuItem");
			((ToolStripItem)animatedGifToolStripMenuItem).set_Size(new Size(109, 20));
			((ToolStripItem)animatedGifToolStripMenuItem).set_Text("Animated gif ...   ");
			((ToolStripItem)setAnimatedGIFParametersToolStripMenuItem).set_Name("setAnimatedGIFParametersToolStripMenuItem");
			((ToolStripItem)setAnimatedGIFParametersToolStripMenuItem).set_Size(new Size(200, 22));
			((ToolStripItem)setAnimatedGIFParametersToolStripMenuItem).set_Text("Animated GIF settings");
			((ToolStripItem)setAnimatedGIFParametersToolStripMenuItem).add_Click((EventHandler)setAnimatedGIFParametersToolStripMenuItem_Click);
			((ToolStripItem)create10To5YearsToolStripMenuItem).set_Name("create10To5YearsToolStripMenuItem");
			((ToolStripItem)create10To5YearsToolStripMenuItem).set_Size(new Size(200, 22));
			((ToolStripItem)create10To5YearsToolStripMenuItem).set_Text("Create (-10  to +5 years)");
			((ToolStripItem)create10To5YearsToolStripMenuItem).add_Click((EventHandler)create10To5YearsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(78, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help      ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			chklstCats.set_CheckOnClick(true);
			((Control)chklstCats).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chklstCats).set_FormattingEnabled(true);
			((ObjectCollection)chklstCats.get_Items()).AddRange(new object[21]
			{
				"I/350   Gaia EDR3", "I/345   GaiaDR2 ", "I/337   Gaia DR1 & TGAS)", "I/339   HSOY", "I/311   Hipparcos 2", "I/264   FK6", "I/322A  UCAC4", "I/259   Tycho-2", "I/329   URAT1", "I/315   UCAC3 ",
				"I/289   UCAC2  ", "I/317   PPMXL ", "I/312   PPMX", "I/320   SPM4", "I/327   CMC15", "II/246  2MASS", "II/321  IPHAS2", "I/146   PPM", "I/284   USNO-B1", "I/297   NOMAD",
				"I/304   CMC14"
			});
			((Control)chklstCats).set_Location(new Point(6, 190));
			((Control)chklstCats).set_Name("chklstCats");
			((Control)chklstCats).set_Size(new Size(242, 184));
			((Control)chklstCats).set_TabIndex(2);
			chklstCats.add_Click((EventHandler)chklstCats_Click);
			((Control)chklstCats).add_KeyDown(new KeyEventHandler(chklstCats_KeyDown));
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(44, 174));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(166, 13));
			((Control)label18).set_TabIndex(1);
			((Control)label18).set_Text("Select catalogues to search");
			((Control)lstPositions).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstPositions).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstPositions).set_FormattingEnabled(true);
			lstPositions.set_HorizontalExtent(1790);
			lstPositions.set_HorizontalScrollbar(true);
			lstPositions.set_ItemHeight(14);
			((Control)lstPositions).set_Location(new Point(12, 389));
			((Control)lstPositions).set_Name("lstPositions");
			lstPositions.set_ScrollAlwaysVisible(true);
			((Control)lstPositions).set_Size(new Size(1037, 172));
			((Control)lstPositions).set_TabIndex(24);
			((Control)lstPositions).add_MouseDown(new MouseEventHandler(lstPositions_MouseDown));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)showStarInVizieRToolStripMenuItem });
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(173, 26));
			((ToolStripItem)showStarInVizieRToolStripMenuItem).set_Image((Image)Resources.vizier_40x35);
			((ToolStripItem)showStarInVizieRToolStripMenuItem).set_Name("showStarInVizieRToolStripMenuItem");
			((ToolStripItem)showStarInVizieRToolStripMenuItem).set_Size(new Size(172, 22));
			((ToolStripItem)showStarInVizieRToolStripMenuItem).set_Text("Show star in VizieR");
			((ToolStripItem)showStarInVizieRToolStripMenuItem).add_Click((EventHandler)showStarInVizieRToolStripMenuItem_Click);
			updnEpoch.set_DecimalPlaces(3);
			updnEpoch.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnEpoch).set_Location(new Point(394, 190));
			updnEpoch.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnEpoch.set_Minimum(new decimal(new int[4] { 1600, 0, 0, 0 }));
			((Control)updnEpoch).set_Name("updnEpoch");
			((Control)updnEpoch).set_Size(new Size(69, 20));
			((Control)updnEpoch).set_TabIndex(6);
			((UpDownBase)updnEpoch).set_TextAlign((HorizontalAlignment)2);
			updnEpoch.set_Value(new decimal(new int[4] { 2009, 0, 0, 0 }));
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(282, 194));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(110, 13));
			((Control)label17).set_TabIndex(5);
			((Control)label17).set_Text("Epoch for comparison");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(287, 216));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(121, 13));
			((Control)label20).set_TabIndex(8);
			((Control)label20).set_Text("Object diameter (masec)");
			((Control)updnDiameter).set_Location(new Point(410, 212));
			updnDiameter.set_Maximum(new decimal(new int[4] { 52000, 0, 0, 0 }));
			((Control)updnDiameter).set_Name("updnDiameter");
			((Control)updnDiameter).set_Size(new Size(53, 20));
			((Control)updnDiameter).set_TabIndex(9);
			((UpDownBase)updnDiameter).set_TextAlign((HorizontalAlignment)2);
			updnDiameter.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			trackBar1.set_LargeChange(10);
			((Control)trackBar1).set_Location(new Point(508, 56));
			trackBar1.set_Maximum(50);
			trackBar1.set_Minimum(-50);
			((Control)trackBar1).set_Name("trackBar1");
			trackBar1.set_Orientation((Orientation)1);
			((Control)trackBar1).set_Size(new Size(45, 295));
			((Control)trackBar1).set_TabIndex(21);
			trackBar1.set_TickFrequency(5);
			trackBar1.set_Value(20);
			trackBar1.add_ValueChanged((EventHandler)trackBar1_ValueChanged);
			((Control)lblScale).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblScale).set_Location(new Point(475, 348));
			((Control)lblScale).set_Name("lblScale");
			((Control)lblScale).set_Size(new Size(73, 11));
			((Control)lblScale).set_TabIndex(23);
			((Control)lblScale).set_Text("200 mas");
			lblScale.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(495, 24));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(53, 13));
			((Control)label23).set_TabIndex(19);
			((Control)label23).set_Text("Plot scale");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(306, 260));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(106, 13));
			((Control)label21).set_TabIndex(12);
			((Control)label21).set_Text("PA of object's motion");
			updnMotionPA.set_DecimalPlaces(1);
			((Control)updnMotionPA).set_Location(new Point(414, 256));
			updnMotionPA.set_Maximum(new decimal(new int[4] { 400, 0, 0, 0 }));
			updnMotionPA.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnMotionPA).set_Name("updnMotionPA");
			((Control)updnMotionPA).set_Size(new Size(49, 20));
			((Control)updnMotionPA).set_TabIndex(13);
			updnMotionPA.set_Value(new decimal(new int[4] { 90, 0, 0, 0 }));
			((Control)chkShowMotion).set_AutoSize(true);
			chkShowMotion.set_CheckAlign(ContentAlignment.MiddleRight);
			chkShowMotion.set_Checked(true);
			chkShowMotion.set_CheckState((CheckState)1);
			((Control)chkShowMotion).set_Location(new Point(251, 275));
			((Control)chkShowMotion).set_Name("chkShowMotion");
			((Control)chkShowMotion).set_Size(new Size(127, 17));
			((Control)chkShowMotion).set_TabIndex(14);
			((Control)chkShowMotion).set_Text("Show:  path of object");
			((ButtonBase)chkShowMotion).set_UseVisualStyleBackColor(true);
			chkShowMotion.add_CheckedChanged((EventHandler)chkShowMotion_CheckedChanged);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(567, 28));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(283, 13));
			((Control)label22).set_TabIndex(22);
			((Control)label22).set_Text("Sky plot.    Displace path on Earth in the opposite direction");
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(430, 2));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(635, 22));
			((Control)label24).set_TabIndex(26);
			((Control)label24).set_Text("This functionality is best accessed from the Asteroid prediction plot");
			((Control)cmdReDisplay).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdReDisplay).set_Location(new Point(388, 313));
			((Control)cmdReDisplay).set_Name("cmdReDisplay");
			((Control)cmdReDisplay).set_Size(new Size(86, 42));
			((Control)cmdReDisplay).set_TabIndex(16);
			((Control)cmdReDisplay).set_Text("re-display positions");
			((ButtonBase)cmdReDisplay).set_UseVisualStyleBackColor(true);
			((Control)cmdReDisplay).add_Click((EventHandler)cmdReDisplay_Click);
			((Control)cmdNow).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdNow).set_Location(new Point(469, 190));
			((Control)cmdNow).set_Name("cmdNow");
			((Control)cmdNow).set_Size(new Size(33, 20));
			((Control)cmdNow).set_TabIndex(7);
			((Control)cmdNow).set_Text("Now");
			((ButtonBase)cmdNow).set_TextAlign(ContentAlignment.TopLeft);
			((ButtonBase)cmdNow).set_UseVisualStyleBackColor(true);
			((Control)cmdNow).add_Click((EventHandler)cmdNow_Click);
			((Control)chkScale).set_AutoSize(true);
			((Control)chkScale).set_Location(new Point(499, 40));
			((Control)chkScale).set_Name("chkScale");
			((Control)chkScale).set_Size(new Size(43, 17));
			((Control)chkScale).set_TabIndex(20);
			((Control)chkScale).set_Text("x20");
			((ButtonBase)chkScale).set_UseVisualStyleBackColor(true);
			chkScale.add_CheckedChanged((EventHandler)chkScale_CheckedChanged);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(286, 238));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(123, 13));
			((Control)label25).set_TabIndex(10);
			((Control)label25).set_Text("Maximum duration (secs)");
			((Control)txtDuration).set_Location(new Point(412, 234));
			((Control)txtDuration).set_Name("txtDuration");
			((Control)txtDuration).set_Size(new Size(51, 20));
			((Control)txtDuration).set_TabIndex(11);
			((Control)txtDuration).set_Text("5");
			txtDuration.set_TextAlign((HorizontalAlignment)2);
			((Control)cmdGoogleSky).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGoogleSky).set_Location(new Point(282, 361));
			((Control)cmdGoogleSky).set_Name("cmdGoogleSky");
			((Control)cmdGoogleSky).set_Size(new Size(75, 22));
			((Control)cmdGoogleSky).set_TabIndex(17);
			((Control)cmdGoogleSky).set_Text("GoogleSky");
			((ButtonBase)cmdGoogleSky).set_UseVisualStyleBackColor(true);
			((Control)cmdGoogleSky).add_Click((EventHandler)cmdGoogleSky_Click);
			((Control)cmdDoubles).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDoubles).set_Location(new Point(384, 361));
			((Control)cmdDoubles).set_Name("cmdDoubles");
			((Control)cmdDoubles).set_Size(new Size(95, 22));
			((Control)cmdDoubles).set_TabIndex(18);
			((Control)cmdDoubles).set_Text("WDS/IF/Vars");
			((ButtonBase)cmdDoubles).set_UseVisualStyleBackColor(true);
			((Control)cmdDoubles).add_Click((EventHandler)cmdDoubles_Click);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(12, 377));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(204, 12));
			((Control)label26).set_TabIndex(27);
			((Control)label26).set_Text("Right-click to show star entry in VizieR");
			((Control)picLegend).set_BackColor(Color.White);
			((Control)picLegend).set_Location(new Point(893, 44));
			((Control)picLegend).set_Name("picLegend");
			((Control)picLegend).set_Size(new Size(140, 335));
			picLegend.set_TabIndex(11);
			picLegend.set_TabStop(false);
			((Control)picStarPlot).set_BackColor(Color.Black);
			((Control)picStarPlot).set_Location(new Point(548, 44));
			((Control)picStarPlot).set_Name("picStarPlot");
			((Control)picStarPlot).set_Size(new Size(335, 335));
			picStarPlot.set_TabIndex(10);
			picStarPlot.set_TabStop(false);
			((Control)picStarPlot).add_MouseMove(new MouseEventHandler(picStarPlot_MouseMove));
			((Control)chkStarUncertainty).set_AutoSize(true);
			chkStarUncertainty.set_CheckAlign(ContentAlignment.MiddleRight);
			chkStarUncertainty.set_Checked(Settings.Default.ShowStarUncertainty);
			chkStarUncertainty.set_CheckState((CheckState)1);
			((Control)chkStarUncertainty).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "ShowStarUncertainty", true, (DataSourceUpdateMode)1));
			((Control)chkStarUncertainty).set_Location(new Point(298, 290));
			((Control)chkStarUncertainty).set_Name("chkStarUncertainty");
			((Control)chkStarUncertainty).set_Size(new Size(80, 17));
			((Control)chkStarUncertainty).set_TabIndex(28);
			((Control)chkStarUncertainty).set_Text("Star Uncert");
			((ButtonBase)chkStarUncertainty).set_UseVisualStyleBackColor(true);
			chkStarUncertainty.add_CheckedChanged((EventHandler)chkStarUncertainty_CheckedChanged);
			((Control)chkParallax).set_AutoSize(true);
			chkParallax.set_CheckAlign(ContentAlignment.MiddleRight);
			chkParallax.set_Checked(true);
			chkParallax.set_CheckState((CheckState)1);
			((Control)chkParallax).set_Location(new Point(386, 282));
			((Control)chkParallax).set_Name("chkParallax");
			((Control)chkParallax).set_Size(new Size(100, 17));
			((Control)chkParallax).set_TabIndex(29);
			((Control)chkParallax).set_Text("Include parallax");
			((ButtonBase)chkParallax).set_UseVisualStyleBackColor(true);
			chkParallax.add_CheckedChanged((EventHandler)chkParallax_CheckedChanged);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1061, 566));
			((Control)this).get_Controls().Add((Control)(object)chkParallax);
			((Control)this).get_Controls().Add((Control)(object)chkStarUncertainty);
			((Control)this).get_Controls().Add((Control)(object)cmdDoubles);
			((Control)this).get_Controls().Add((Control)(object)cmdGoogleSky);
			((Control)this).get_Controls().Add((Control)(object)txtDuration);
			((Control)this).get_Controls().Add((Control)(object)label25);
			((Control)this).get_Controls().Add((Control)(object)chkScale);
			((Control)this).get_Controls().Add((Control)(object)cmdNow);
			((Control)this).get_Controls().Add((Control)(object)cmdReDisplay);
			((Control)this).get_Controls().Add((Control)(object)label24);
			((Control)this).get_Controls().Add((Control)(object)label22);
			((Control)this).get_Controls().Add((Control)(object)label21);
			((Control)this).get_Controls().Add((Control)(object)chkShowMotion);
			((Control)this).get_Controls().Add((Control)(object)updnMotionPA);
			((Control)this).get_Controls().Add((Control)(object)label23);
			((Control)this).get_Controls().Add((Control)(object)label20);
			((Control)this).get_Controls().Add((Control)(object)updnDiameter);
			((Control)this).get_Controls().Add((Control)(object)picLegend);
			((Control)this).get_Controls().Add((Control)(object)picStarPlot);
			((Control)this).get_Controls().Add((Control)(object)label17);
			((Control)this).get_Controls().Add((Control)(object)updnEpoch);
			((Control)this).get_Controls().Add((Control)(object)label18);
			((Control)this).get_Controls().Add((Control)(object)label15);
			((Control)this).get_Controls().Add((Control)(object)updnRadius);
			((Control)this).get_Controls().Add((Control)(object)cmdListPositions);
			((Control)this).get_Controls().Add((Control)(object)chklstCats);
			((Control)this).get_Controls().Add((Control)(object)grpStar);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lblScale);
			((Control)this).get_Controls().Add((Control)(object)lstPositions);
			((Control)this).get_Controls().Add((Control)(object)trackBar1);
			((Control)this).get_Controls().Add((Control)(object)label26);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationStarsCompare", true, (DataSourceUpdateMode)1));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationStarsCompare);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(900, 430));
			((Control)this).set_Name("CompareStarCats");
			((Control)this).set_Text("Compare Star Catalogues");
			((Form)this).add_Load((EventHandler)CompareStarCats_Load);
			((Control)this).add_Resize((EventHandler)CompareStarCats_Resize);
			((Control)grpStar).ResumeLayout(false);
			((Control)grpStar).PerformLayout();
			((Control)panelCoordinates).ResumeLayout(false);
			((Control)panelCoordinates).PerformLayout();
			((Control)panelSelectCatalog).ResumeLayout(false);
			((Control)panelSelectCatalog).PerformLayout();
			((Control)panelUCAC4).ResumeLayout(false);
			((Control)panelUCAC4).PerformLayout();
			((Control)panelTycho2).ResumeLayout(false);
			((Control)panelTycho2).PerformLayout();
			((Control)panelHip).ResumeLayout(false);
			((Control)panelHip).PerformLayout();
			((Control)panelB1).ResumeLayout(false);
			((Control)panelB1).PerformLayout();
			((ISupportInitialize)updnRadius).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)contextMenuStrip1).ResumeLayout(false);
			((ISupportInitialize)updnEpoch).EndInit();
			((ISupportInitialize)updnDiameter).EndInit();
			((ISupportInitialize)trackBar1).EndInit();
			((ISupportInitialize)updnMotionPA).EndInit();
			((ISupportInitialize)picLegend).EndInit();
			((ISupportInitialize)picStarPlot).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
