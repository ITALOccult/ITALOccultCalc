using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult.Lunar_Observations
{
	public class EventTimeFromStarAltitude : Form
	{
		private string OldStars = Utilities.AppPath + "\\Resource Files\\OldStars.txt";

		private string OldOldStars = Utilities.AppPath + "\\Resource Files\\OldStars.old";

		private string OldObservers = Utilities.AppPath + "\\Resource Files\\OldObservers.txt";

		private string OldOldObservers = Utilities.AppPath + "\\Resource Files\\OldObservers.old";

		private string NoRefractionCorrectionApplied = ":";

		private string RefractionCorrectionApplied = ".";

		private string ObservedEvent = "_";

		private string CommentTag = "`";

		private string ClockCorrection = "~";

		private string[] Observers;

		private const double Radian = 180.0 / Math.PI;

		private double LongD;

		private double LatD;

		private double Alt;

		private double UTRise_hr;

		private double UTSet_hr;

		private double UT_of_LocalTransit_hrs;

		private double LocalMeanTimeRise;

		private double LocalMeanTimeSet;

		private double LocalMeanTransit_hrs;

		private double LocalApparentTimeRise;

		private double LocalApparentTimeSet;

		private double LocalApparentTransit_hrs;

		private double dTperArcMin;

		private int UTDayRise;

		private int UTDaySet;

		private int UTDayTransit;

		private int LocalMeanDayRise;

		private int LocalMeanDaySet;

		private int LocalMeanTransitDay;

		private int LocalApparentDayRise;

		private int LocalApparentDaySet;

		private int LocalApparentTransitDay;

		private int HipNum;

		private int PlanetNum = 5;

		private double SunRA;

		private double SunDec;

		private double SunD;

		private double EqnTimeHrs0;

		private double EqnTimeHrs1;

		private double dEqnTimeHrs;

		private bool RiseInPrevious;

		private bool SetInFollowing;

		private double EThrs;

		private double UT;

		private double StarAlt_deg;

		private double Refraction;

		private double UTofEventHrs;

		private double ClockCorrnConstant;

		private double ClockCorrectionRate;

		private double ClockCorrectionZeroT;

		private bool CompIsPlanet;

		private double CurrentEventUTHrs;

		private double CurrentTransitUTHrs;

		private double CurrentRiseUTHrs;

		private double CurrentSetUTHrs;

		private bool Loaded;

		private IContainer components;

		private ComboBox cmbObserver;

		private Label label1;

		private ComboBox cmbStarForAltitude;

		private Label label3;

		private Label label4;

		private TextBox txtStarAltDeg;

		private TextBox txtStarAltMin;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label label8;

		private Label label9;

		internal ComboBox cmbDay;

		internal ComboBox cmbMonth;

		internal NumericUpDown updnYear;

		private Label label11;

		private Label label12;

		private Label label13;

		internal TextBox txtLatS;

		internal TextBox txtLatM;

		internal TextBox txtLatD;

		internal TextBox txtLongS;

		internal TextBox txtLongM;

		internal TextBox txtLongD;

		internal TextBox txtAltitude;

		private Label label24;

		private Label label25;

		private Label label20;

		private Label label10;

		private Label label14;

		private Label label15;

		private Label label16;

		private TextBox txtStarCoords;

		private Label label17;

		private Label lblUTTransit;

		private Panel panelAlt;

		private Label label19;

		private Label label21;

		private Label label22;

		private Label lblUTSet;

		private Label lblUTRise;

		private Label label18;

		private Label label23;

		private Label lblMeanRise;

		private Label lblMeanSet;

		private Label lblMeanTransit;

		private Label label29;

		private Label lblApparentRise;

		private Label lblApparentSet;

		private Label lblApparentTransit;

		internal TextBox txtS;

		internal TextBox txtM;

		internal TextBox txtH;

		private Label label26;

		private Label label27;

		private Label label28;

		private Panel panel1;

		private Label label30;

		private Label lblEventUT;

		private RadioButton optApparent;

		private RadioButton optMean;

		private Label label31;

		private Label lbldT_RiseSet;

		private Label label35;

		private Label lblEventCorrectedUT;

		private Label label32;

		private Label label2;

		private Label lblRefraction;

		private Label label36;

		private Label lblCorrectedAltitude;

		private Label label37;

		private CheckBox chkCorrectForRefraction;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem downloadUpdatesToolStripMenuItem;

		private ToolStripMenuItem starNamesToolStripMenuItem;

		private ToolStripMenuItem observersToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Label label38;

		private Label label39;

		private Label label40;

		private Label label41;

		private Label lblResidual;

		private Label label43;

		private Label label42;

		private TextBox txtStar;

		internal TextBox txtDay;

		private Label label44;

		private Panel panel2;

		private Panel pnlSiteCoords;

		private Panel panel3;

		private Panel panel4;

		private Button cmdDeleteItemFromList;

		private Label label46;

		private Button cmdAddRise;

		private Button cmdAddSet;

		private Button cmdAddTransit;

		private Button cmdDuplicate;

		private Label lblMean_NoRefractionCorrn;

		private Label label45;

		private Label lblLinearNoRefractionCorrn;

		private Label label47;

		private Label label52;

		private Label label53;

		private Label label54;

		private Label label55;

		private Label label48;

		private Label label49;

		private Label label50;

		private Label label51;

		private Panel panel5;

		private CheckedListBox clbObservations;

		private Label lblMeanCorrectedForRefraction;

		private Label lblLinearCorrectedForRefraction;

		private Panel panel7;

		private Label label60;

		private RadioButton optLinearFit;

		private RadioButton optMeanFit;

		private Panel panel6;

		private Label label59;

		private RadioButton optRefractionCorrected;

		private RadioButton optRefractionNotCorrected;

		private Label label58;

		private Label label57;

		private Label label56;

		private Button cmdAddToOutput;

		private Label lblClockCorrection;

		private Label label33;

		private Label label34;

		private Button cmdAddClockCorrnToList;

		private Button cmdCopy;

		private Button cmdPastOutput;

		private Button cmdSave;

		private Label label61;

		private Label label62;

		private Button cmdAddComment;

		private ToolStripMenuItem resetDateObservationsToolStripMenuItem;

		private Button cmdTFApparentSet;

		private Button cmdTFApparentRise;

		private Button cmdTFApparentTransit;

		private Button cmdTFMeanSet;

		private Button cmdTFMeanRise;

		private Button cmdTFMeanTransit;

		private Label lbldeltaT;

		private Label label63;

		public EventTimeFromStarAltitude()
		{
			InitializeComponent();
		}

		private void EventTimeFromStarAltitude_Load(object sender, EventArgs e)
		{
			//IL_0039: Unknown result type (might be due to invalid IL or missing references)
			//IL_0083: Unknown result type (might be due to invalid IL or missing references)
			//IL_00ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_00f7: Unknown result type (might be due to invalid IL or missing references)
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (!File.Exists(OldStars))
			{
				if (!Utilities.InternetIsAvailable())
				{
					MessageBox.Show("This function cannot proceed until a file is downloaded from the Occult server. \r\n\r\nTry again after you have connected to the internet.", "No internet");
					((Form)this).Close();
				}
				http.DownloadHTTP(Settings.Default.OccultServer, "oldstars.zip", Utilities.AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: false);
				if (!File.Exists(OldStars))
				{
					MessageBox.Show("The download failed.\r\n\r\nTry again later.", "No internet");
					((Form)this).Close();
				}
			}
			if (!File.Exists(OldObservers))
			{
				if (!Utilities.InternetIsAvailable())
				{
					MessageBox.Show("This function cannot proceed until a file is downloaded from the Occult server. \r\n\r\nTry again after you have connected to the internet.", "No internet");
					((Form)this).Close();
				}
				http.DownloadHTTP(Settings.Default.OccultServer, "oldobservers.zip", Utilities.AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: false);
				if (!File.Exists(OldStars))
				{
					MessageBox.Show("The download failed.\r\n\r\nTry again later.", "No internet");
					((Form)this).Close();
				}
			}
			ReadObservers();
			ReadStars();
			((ListControl)cmbMonth).set_SelectedIndex(0);
			((ListControl)cmbDay).set_SelectedIndex(1);
			Loaded = true;
			UpdateCalc();
		}

		private void ReadObservers()
		{
			cmbObserver.get_Items().Clear();
			using (StreamReader streamReader = new StreamReader(OldObservers))
			{
				Observers = streamReader.ReadToEnd().Split(new string[3] { "\r\n", "\r", "\n" }, StringSplitOptions.None);
			}
			for (int i = 0; i < Observers.Length; i++)
			{
				cmbObserver.get_Items().Add((object)Observers[i].Substring(0, 22));
			}
			if (cmbObserver.get_Items().get_Count() > Settings.Default.HistoricalObserver)
			{
				((ListControl)cmbObserver).set_SelectedIndex(Settings.Default.HistoricalObserver);
			}
		}

		private void ReadStars()
		{
			cmbStarForAltitude.get_Items().Clear();
			using (StreamReader streamReader = new StreamReader(OldStars))
			{
				do
				{
					cmbStarForAltitude.get_Items().Add((object)streamReader.ReadLine());
				}
				while (!streamReader.EndOfStream);
			}
			cmbStarForAltitude.set_Sorted(true);
			((ListControl)cmbStarForAltitude).set_SelectedIndex(0);
		}

		private void cmbObserver_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.HistoricalObserver = ((ListControl)cmbObserver).get_SelectedIndex();
			((Control)pnlSiteCoords).set_Enabled(((ListControl)cmbObserver).get_SelectedIndex() < 1);
			int selectedIndex = ((ListControl)cmbObserver).get_SelectedIndex();
			if (selectedIndex >= 1)
			{
				((Control)txtLongD).set_Text(Observers[selectedIndex].Substring(24, 4));
				((Control)txtLongM).set_Text(Observers[selectedIndex].Substring(28, 2));
				((Control)txtLongS).set_Text(Observers[selectedIndex].Substring(30, 4));
				((Control)txtLatD).set_Text(Observers[selectedIndex].Substring(35, 3));
				((Control)txtLatM).set_Text(Observers[selectedIndex].Substring(38, 2));
				((Control)txtLatS).set_Text(Observers[selectedIndex].Substring(40, 4));
				((Control)txtAltitude).set_Text(Observers[selectedIndex].Substring(47, 4));
				UpdateCalc();
			}
		}

		private void updnYear_ValueChanged(object sender, EventArgs e)
		{
			UpdateCalc();
		}

		private void cmbMonth_SelectedIndexChanged(object sender, EventArgs e)
		{
			UpdateCalc();
		}

		private void starNamesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This function cannot proceed unless you are connected to the Internet.", "No internet");
				return;
			}
			if (File.Exists(OldOldStars))
			{
				File.Delete(OldOldStars);
			}
			File.Move(OldStars, OldOldStars);
			if (http.DownloadHTTP(Settings.Default.OccultServer, "oldstars.zip", Utilities.AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: true))
			{
				ReadStars();
			}
		}

		private void chkCorrectForRefraction_CheckedChanged(object sender, EventArgs e)
		{
			UpdateCalc();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void cmdClear_Click(object sender, EventArgs e)
		{
			((ObjectCollection)clbObservations.get_Items()).Clear();
			updnYear.set_Value(1675m);
			((ListControl)cmbMonth).set_SelectedIndex(0);
			((ListControl)cmbDay).set_SelectedIndex(1);
			((Control)txtStar).set_Text("");
		}

		private void cmbDay_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ObjectCollection)clbObservations.get_Items()).get_Count() < 1)
			{
				((Control)txtDay).set_Text(cmbDay.get_Items().get_Item(((ListControl)cmbDay).get_SelectedIndex()).ToString());
			}
			UpdateCalc();
		}

		private void cmbStarForAltitude_SelectedIndexChanged(object sender, EventArgs e)
		{
			UpdateCalc();
		}

		private void UpdateCalc()
		{
			//IL_026f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			if (!Loaded)
			{
				return;
			}
			double RA = 0.0;
			double num = 0.0;
			double Dec = 0.0;
			double num2 = 0.0;
			int year = (int)updnYear.get_Value();
			int month = ((ListControl)cmbMonth).get_SelectedIndex() + 1;
			int selectedIndex = ((ListControl)cmbDay).get_SelectedIndex();
			UTDayRise = (UTDaySet = (UTDayTransit = (LocalMeanDayRise = (LocalMeanDaySet = (LocalMeanTransitDay = (LocalApparentDayRise = (LocalApparentDaySet = (LocalApparentTransitDay = selectedIndex))))))));
			double num3 = Utilities.JD_from_Date(year, month, selectedIndex);
			LongD = double.Parse(((Control)txtLongD).get_Text().Replace("-", "").Replace("+", "")) + double.Parse(((Control)txtLongM).get_Text()) / 60.0 + double.Parse(((Control)txtLongS).get_Text()) / 3600.0;
			if (((Control)txtLongD).get_Text().Contains("-"))
			{
				LongD = 0.0 - LongD;
			}
			LatD = double.Parse(((Control)txtLatD).get_Text().Replace("-", "").Replace("+", "")) + double.Parse(((Control)txtLatM).get_Text()) / 60.0 + double.Parse(((Control)txtLatS).get_Text()) / 3600.0;
			if (((Control)txtLatD).get_Text().Contains("-"))
			{
				LatD = 0.0 - LatD;
			}
			if (((ListControl)cmbStarForAltitude).get_SelectedIndex() < 0)
			{
				return;
			}
			((Control)label17).set_Text("Star coordinates ....");
			((Control)txtStarCoords).set_Text("");
			string text = cmbStarForAltitude.get_Items().get_Item(((ListControl)cmbStarForAltitude).get_SelectedIndex()).ToString();
			CompIsPlanet = false;
			int num4 = text.IndexOf("(") + 4;
			int num5 = text.IndexOf(")") - num4;
			if (num5 < 2)
			{
				MessageBox.Show("HIP number not specified for " + text);
			}
			else if (text.Substring(num4, num5).Contains("net"))
			{
				CompIsPlanet = true;
				PlanetNum = int.Parse(text.Substring(num4 + 3, num5 - 3));
				if (!double.TryParse(((Control)txtDay).get_Text(), out var result))
				{
					result = 1.0;
				}
				Utilities.PlanetGeocentric(Utilities.JD_from_Date(year, month, result + CurrentEventUTHrs / 24.0), PlanetNum, 1E-06, 0, out RA, out Dec, out var _);
				((Control)txtStarCoords).set_Text("Apparent : " + Utilities.DEGtoDMS(RA * (180.0 / Math.PI) / 15.0, 2, 3, MinutesOnly: false) + " " + Utilities.DEGtoDMS(Dec * (180.0 / Math.PI), 3, 2, MinutesOnly: false));
				((Control)label17).set_Text("Coordinates for " + text);
				double num6 = Utilities.SiderealTime_deg(num3, Apparent: false);
				Utilities.PlanetGeocentric(num3, 3, 1E-06, 0, out SunRA, out SunDec, out SunD);
				EqnTimeHrs0 = (SunRA * (180.0 / Math.PI) - num6) / 15.0 - 12.0;
				double num7 = Utilities.SiderealTime_deg(num3 + 1.0, Apparent: false);
				Utilities.PlanetGeocentric(num3 + 1.0, 3, 1E-06, 0, out SunRA, out SunDec, out SunD);
				EqnTimeHrs1 = (SunRA * (180.0 / Math.PI) - num7) / 15.0 - 12.0;
				dEqnTimeHrs = (EqnTimeHrs1 - EqnTimeHrs0) / 24.0;
				if (EqnTimeHrs0 > 12.0)
				{
					EqnTimeHrs0 -= 24.0;
				}
				if (EqnTimeHrs0 < -12.0)
				{
					EqnTimeHrs0 += 24.0;
				}
				if (EqnTimeHrs1 > 12.0)
				{
					EqnTimeHrs1 -= 24.0;
				}
				if (EqnTimeHrs1 < -12.0)
				{
					EqnTimeHrs1 += 24.0;
				}
				int result2;
				bool num8 = int.TryParse(((Control)txtStarAltDeg).get_Text(), out result2);
				double result3;
				bool flag = double.TryParse(((Control)txtStarAltMin).get_Text(), out result3);
				if (!(!num8 || !flag))
				{
					StarAlt_deg = (double)result2 + result3 / 60.0;
					Refraction = Utilities.Refraction_deg(StarAlt_deg, 1010.0, 5.0);
					((Control)lblRefraction).set_Text(string.Format("{0,2:f1}'", Refraction * 60.0));
					if (chkCorrectForRefraction.get_Checked())
					{
						StarAlt_deg -= Refraction;
					}
					((Control)lblCorrectedAltitude).set_Text(Utilities.DEGtoDMS(StarAlt_deg, 2, 1, MinutesOnly: true));
					Utilities.RiseSet_UT(StarAlt_deg + 1.0 / 60.0, RA, 0.0, Dec, 0.0, num3, LongD, LatD, out var TRise_hrs, out RiseInPrevious, out var _, out SetInFollowing, out UT_of_LocalTransit_hrs);
					Utilities.RiseSet_UT(StarAlt_deg, RA, 0.0, Dec, 0.0, num3, LongD, LatD, out UTRise_hr, out RiseInPrevious, out UTSet_hr, out SetInFollowing, out UT_of_LocalTransit_hrs);
					CurrentTransitUTHrs = UT_of_LocalTransit_hrs;
					CurrentRiseUTHrs = UTRise_hr;
					if (RiseInPrevious)
					{
						CurrentRiseUTHrs -= 24.0;
					}
					CurrentSetUTHrs = UTSet_hr;
					if (SetInFollowing)
					{
						CurrentSetUTHrs += 24.0;
					}
					if (RiseInPrevious)
					{
						UTDayRise = (LocalMeanDayRise = (LocalApparentDayRise = selectedIndex - 1));
					}
					if (SetInFollowing)
					{
						UTDaySet = (LocalMeanDaySet = (LocalApparentDaySet = selectedIndex + 1));
					}
					double num9 = (dTperArcMin = (TRise_hrs - UTRise_hr) * 3600.0);
					_ = UTSet_hr;
					LocalMeanTransit_hrs = UT_of_LocalTransit_hrs + LongD / 15.0 - 12.0;
					LocalApparentTransit_hrs = UT_of_LocalTransit_hrs + LongD / 15.0 - 12.0 - EqnTimeHrs0 - LocalMeanTransit_hrs * dEqnTimeHrs;
					LocalApparentTimeRise = UTRise_hr + LongD / 15.0 - 12.0 - EqnTimeHrs0 - LocalMeanTimeRise * dEqnTimeHrs;
					LocalApparentTimeSet = UTSet_hr + LongD / 15.0 - 12.0 - EqnTimeHrs0 - LocalMeanTimeSet * dEqnTimeHrs;
					LocalMeanTimeRise = UTRise_hr + LongD / 15.0 - 12.0;
					LocalMeanTimeSet = UTSet_hr + LongD / 15.0 - 12.0;
					if (LocalMeanTransit_hrs < 0.0)
					{
						LocalMeanTransit_hrs += 24.0;
						LocalMeanTransitDay--;
					}
					if (LocalMeanTransit_hrs > 24.0)
					{
						LocalMeanTransit_hrs -= 24.0;
						LocalMeanTransitDay++;
					}
					if (LocalMeanTimeRise < 0.0)
					{
						LocalMeanTimeRise += 24.0;
						LocalMeanDayRise--;
					}
					if (LocalMeanTimeRise > 24.0)
					{
						LocalMeanTimeRise -= 24.0;
						LocalMeanDayRise++;
					}
					if (LocalMeanTimeSet < 0.0)
					{
						LocalMeanTimeSet += 24.0;
						LocalMeanDaySet--;
					}
					if (LocalMeanTimeSet > 24.0)
					{
						LocalMeanTimeSet -= 24.0;
						LocalMeanDaySet++;
					}
					if (LocalApparentTransit_hrs < 0.0)
					{
						LocalApparentTransit_hrs += 24.0;
						LocalApparentTransitDay--;
					}
					if (LocalApparentTransit_hrs > 24.0)
					{
						LocalApparentTransit_hrs -= 24.0;
						LocalApparentTransitDay++;
					}
					if (LocalApparentTimeRise < 0.0)
					{
						LocalApparentTimeRise += 24.0;
						LocalApparentDayRise--;
					}
					if (LocalApparentTimeRise > 24.0)
					{
						LocalApparentTimeRise -= 24.0;
						LocalApparentDayRise++;
					}
					if (LocalApparentTimeSet < 0.0)
					{
						LocalApparentTimeSet += 24.0;
						LocalApparentDaySet--;
					}
					if (LocalApparentTimeSet > 24.0)
					{
						LocalApparentTimeSet -= 24.0;
						LocalApparentDaySet++;
					}
					((Control)lblUTTransit).set_Text(string.Format("{0,2}  ", selectedIndex) + Utilities.DEGtoDMS(UT_of_LocalTransit_hrs, 2, 0, MinutesOnly: false));
					((Control)lblMeanTransit).set_Text(string.Format("{0,2}  ", LocalMeanTransitDay) + Utilities.DEGtoDMS(LocalMeanTransit_hrs, 2, 0, MinutesOnly: false));
					((Control)lblApparentTransit).set_Text(string.Format("{0,2}  ", LocalApparentTransitDay) + Utilities.DEGtoDMS(LocalApparentTransit_hrs, 2, 0, MinutesOnly: false));
					if (UTRise_hr == UTSet_hr)
					{
						Label obj = lblUTRise;
						Label obj2 = lblMeanRise;
						Label obj3 = lblApparentRise;
						Label obj4 = lblUTSet;
						Label obj5 = lblMeanSet;
						string text2;
						((Control)lblApparentSet).set_Text(text2 = " -  -- -- --");
						string text3;
						((Control)obj5).set_Text(text3 = text2);
						string text4;
						((Control)obj4).set_Text(text4 = text3);
						string text5;
						((Control)obj3).set_Text(text5 = text4);
						string text6;
						((Control)obj2).set_Text(text6 = text5);
						((Control)obj).set_Text(text6);
					}
					else
					{
						((Control)lblUTRise).set_Text(string.Format("{0,2}  ", UTDayRise) + Utilities.DEGtoDMS(UTRise_hr, 2, 0, MinutesOnly: false));
						((Control)lblMeanRise).set_Text(string.Format("{0,2}  ", LocalMeanDayRise) + Utilities.DEGtoDMS(LocalMeanTimeRise, 2, 0, MinutesOnly: false));
						((Control)lblApparentRise).set_Text(string.Format("{0,2}  ", LocalApparentDayRise) + Utilities.DEGtoDMS(LocalApparentTimeRise, 2, 0, MinutesOnly: false));
						((Control)lblUTSet).set_Text(string.Format("{0,2}  ", UTDaySet) + Utilities.DEGtoDMS(UTSet_hr, 2, 0, MinutesOnly: false));
						((Control)lblMeanSet).set_Text(string.Format("{0,2}  ", LocalMeanDaySet) + Utilities.DEGtoDMS(LocalMeanTimeSet, 2, 0, MinutesOnly: false));
						((Control)lblApparentSet).set_Text(string.Format("{0,2}  ", LocalApparentDaySet) + Utilities.DEGtoDMS(LocalApparentTimeSet, 2, 0, MinutesOnly: false));
						((Control)lbldT_RiseSet).set_Text(string.Format("±{0,3:f1} secs", num9));
					}
				}
			}
			else
			{
				HipNum = int.Parse(text.Substring(num4, num5));
				GetStarPosition.GetHipparcosPosition(HipNum, out RA, out Dec, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var _);
				MessageBox.Show(text + " not found");
			}
		}

		private void txtStarAltDeg_TextChanged(object sender, EventArgs e)
		{
			UpdateCalc();
		}

		private void resetDateObservationsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("You have selected a reset of the date of observations.\r\n\r\nThis option will delete all entered data.\r\n\r\nDo you want to continue?", "Reset date", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				((ObjectCollection)clbObservations.get_Items()).Clear();
				updnYear.set_Value(1675m);
				((ListControl)cmbMonth).set_SelectedIndex(0);
				((ListControl)cmbDay).set_SelectedIndex(1);
				((Control)txtStar).set_Text("");
			}
		}

		private void EventTimeFromStarAltitude_Resize(object sender, EventArgs e)
		{
			((Control)this).set_Width(794);
			if (((Control)this).get_Height() < 658)
			{
				((Control)this).set_Height(658);
			}
			((Control)clbObservations).set_Height(169 + (((Control)this).get_Height() - 658));
			((Control)panel4).set_Height(243 + (((Control)this).get_Height() - 658));
		}

		private void observersToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_0011: Unknown result type (might be due to invalid IL or missing references)
			if (!Utilities.InternetIsAvailable())
			{
				MessageBox.Show("This function cannot proceed unless you are connected to the Internet.", "No internet");
				return;
			}
			if (File.Exists(OldOldObservers))
			{
				File.Delete(OldOldObservers);
			}
			File.Move(OldObservers, OldOldObservers);
			if (http.DownloadHTTP(Settings.Default.OccultServer, "oldobservers.zip", Utilities.AppPath + "\\Resource Files\\", unzip: true, gunzip: false, ShowMessages: true))
			{
				ReadStars();
			}
			ReadObservers();
		}

		private void cmdTFMeanTransit_Click(object sender, EventArgs e)
		{
			TransferRiseSetToEvent(((Control)lblMeanTransit).get_Text(), MeanT: true);
		}

		private void cmdTFMeanRise_Click(object sender, EventArgs e)
		{
			TransferRiseSetToEvent(((Control)lblMeanRise).get_Text(), MeanT: true);
		}

		private void cmdTFMeanSet_Click(object sender, EventArgs e)
		{
			TransferRiseSetToEvent(((Control)lblMeanSet).get_Text(), MeanT: true);
		}

		private void cmdTFApparentTransit_Click(object sender, EventArgs e)
		{
			TransferRiseSetToEvent(((Control)lblApparentTransit).get_Text(), MeanT: false);
		}

		private void cmdTFApparentRise_Click(object sender, EventArgs e)
		{
			TransferRiseSetToEvent(((Control)lblApparentRise).get_Text(), MeanT: false);
		}

		private void cmdTFApparentSet_Click(object sender, EventArgs e)
		{
			TransferRiseSetToEvent(((Control)lblApparentSet).get_Text(), MeanT: false);
		}

		private void TransferRiseSetToEvent(string T, bool MeanT)
		{
			optMean.set_Checked(MeanT);
			optApparent.set_Checked(!MeanT);
			((Control)txtDay).set_Text(T.Substring(0, 2).Trim());
			((Control)txtH).set_Text(T.Substring(4, 2).Trim());
			((Control)txtM).set_Text(T.Substring(7, 2).Trim());
			((Control)txtS).set_Text(T.Substring(10, 2).Trim());
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Event Times from altitudes");
		}

		private void txtStarAltMin_TextChanged(object sender, EventArgs e)
		{
			UpdateCalc();
		}

		private void txtStarAltDeg_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtStarAltDeg).SelectAll();
		}

		private void txtStarAltMin_MouseUp(object sender, MouseEventArgs e)
		{
			((TextBoxBase)txtStarAltMin).SelectAll();
		}

		private void optMean_CheckedChanged(object sender, EventArgs e)
		{
			EventUT();
		}

		private void optApparent_CheckedChanged(object sender, EventArgs e)
		{
			EventUT();
		}

		private void txtDay_TextChanged(object sender, EventArgs e)
		{
			if (CompIsPlanet)
			{
				UpdateCalc();
			}
			EventUT();
		}

		private void txtH_TextChanged(object sender, EventArgs e)
		{
			if (CompIsPlanet)
			{
				UpdateCalc();
			}
			EventUT();
		}

		private void txtM_TextChanged(object sender, EventArgs e)
		{
			EventUT();
		}

		private void txtS_TextChanged(object sender, EventArgs e)
		{
			EventUT();
		}

		private void txtCorrnMins_TextChanged(object sender, EventArgs e)
		{
			EventUT();
		}

		private void txtCorrnSecs_TextChanged(object sender, EventArgs e)
		{
			EventUT();
		}

		private void txtStar_TextChanged(object sender, EventArgs e)
		{
			EventUT();
		}

		private void EventUT()
		{
			double num = 0.0;
			string text = "";
			int result;
			bool num2 = int.TryParse(((Control)txtDay).get_Text(), out result);
			int result2;
			bool flag = int.TryParse(((Control)txtH).get_Text(), out result2);
			int result3;
			bool flag2 = int.TryParse(((Control)txtM).get_Text(), out result3);
			int result4;
			bool flag3 = int.TryParse(((Control)txtS).get_Text(), out result4);
			if (!num2 || !flag || !flag2 || !flag3)
			{
				((Control)lblEventUT).set_Text("00  00 00 00");
				return;
			}
			EThrs = (double)result2 + (double)result3 / 60.0 + (double)result4 / 3600.0;
			if (optMean.get_Checked())
			{
				UT = EThrs - (LongD / 15.0 - 12.0);
			}
			else
			{
				UT = EThrs - (LongD / 15.0 - 12.0 - EqnTimeHrs0);
				UT = EThrs - (LongD / 15.0 - 12.0 - (EqnTimeHrs0 + dEqnTimeHrs * UT));
			}
			CurrentEventUTHrs = UT;
			if (UT < 0.0)
			{
				UT += 24.0;
				result--;
			}
			if (UT > 24.0)
			{
				UT -= 24.0;
				result++;
			}
			((Control)lblEventUT).set_Text(string.Format("{0,2}  ", result) + Utilities.DEGtoDMS(UT, 2, 0, MinutesOnly: false));
			num = (ClockCorrnConstant + ClockCorrectionRate * (CurrentEventUTHrs - ClockCorrectionZeroT)) / 3600.0;
			((Control)lblClockCorrection).set_Text(Utilities.DEGtoDMS(num * 60.0, 3, 0, MinutesOnly: true));
			UTofEventHrs = UT + num;
			((Control)lblEventCorrectedUT).set_Text(string.Format("{0,2}  ", result) + Utilities.DEGtoDMS(UT + num, 2, 0, MinutesOnly: false));
			int result5 = 0;
			if (((Control)txtStar).get_Text().Length > 1)
			{
				text = ((Control)txtStar).get_Text().Substring(0, 1);
				int.TryParse(((Control)txtStar).get_Text().Substring(1), out result5);
				if (text == "P")
				{
					result5 *= 1000;
				}
			}
			if (result5 > 0)
			{
				double num3 = Utilities.JD_from_Date((int)updnYear.get_Value(), ((ListControl)cmbMonth).get_SelectedIndex() + 1, (double)result + (UT + num) / 24.0);
				LunarObservations.ReduceAnObservation(num3, LongD / (180.0 / Math.PI), LatD / (180.0 / Math.PI), Alt, Height_is_MSL: true, 0, text, result5, UseOuterLimbOfSatelliteOrAsteroid: false, PE_ReductionIfBrightStar: false, "", ApplyLimbCorrn: true, VizierReduction: false, out var _, out var Residual, out var _, out var _, out var _, out var _, out var PA, out var _);
				((Control)lblResidual).set_Text(string.Format("{0,6:f1}\" in PA{1,6:f1}°", Residual, PA));
				((Control)lbldeltaT).set_Text(string.Format("{0,1:f0} sec", Utilities.delta_T(num3)));
			}
		}

		private string FormatLine(double Corrn, bool IncludeRefractionCorrection, bool IncludeRateOfChange)
		{
			string text = string.Format(",  HIP{0,6:f0},  ", HipNum);
			if (CompIsPlanet)
			{
				text = "," + Utilities.Planets[PlanetNum].PadLeft(11) + ",  ";
			}
			string text2 = NoRefractionCorrectionApplied;
			if (IncludeRefractionCorrection)
			{
				text2 = RefractionCorrectionApplied;
			}
			string text3 = "0.0'";
			string text4 = string.Format("{0,4:f1}s/'", dTperArcMin);
			if (!IncludeRateOfChange)
			{
				text4 = "...   ";
			}
			string text5 = " (Mean)";
			if (optApparent.get_Checked())
			{
				text5 = " (Appt)";
			}
			if (chkCorrectForRefraction.get_Checked())
			{
				text3 = string.Format("{0,3:f1}'", Refraction * 60.0);
			}
			return text2 + ((Control)txtDay).get_Text().PadLeft(3) + " " + Utilities.DEGtoDMS(EThrs, 2, 0, MinutesOnly: false, IncludeLeadingZeros: false) + text5 + text + Utilities.DEGtoDMS(StarAlt_deg, 2, 1, MinutesOnly: true) + ", " + text3 + ", " + Utilities.DEGtoDMS(UT, 2, 0, MinutesOnly: false) + ", " + text4 + string.Format("|{0,7:f4}", CurrentEventUTHrs) + "|" + string.Format("{0,4:f0}", Corrn * 3600.0);
		}

		private void cmdAddTransit_Click(object sender, EventArgs e)
		{
			double corrn = CurrentTransitUTHrs - CurrentEventUTHrs;
			clbObservations.get_Items().Add((object)FormatLine(corrn, IncludeRefractionCorrection: false, IncludeRateOfChange: false), true);
			SolveForClockCorrection();
		}

		private void cmdAddRise_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < 2; i++)
			{
				chkCorrectForRefraction.set_Checked(i == 1);
				Application.DoEvents();
				double num = CurrentRiseUTHrs - CurrentEventUTHrs;
				if (num < -12.0)
				{
					num += 24.0;
				}
				if (num > 12.0)
				{
					num -= 24.0;
				}
				clbObservations.get_Items().Add((object)FormatLine(num, i == 1, IncludeRateOfChange: true), true);
			}
			SolveForClockCorrection();
		}

		private void cmdAddSet_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < 2; i++)
			{
				chkCorrectForRefraction.set_Checked(i == 1);
				Application.DoEvents();
				double num = CurrentSetUTHrs - CurrentEventUTHrs;
				if (num < -12.0)
				{
					num += 24.0;
				}
				if (num > 12.0)
				{
					num -= 24.0;
				}
				clbObservations.get_Items().Add((object)FormatLine(num, i == 1, IncludeRateOfChange: true), true);
			}
			SolveForClockCorrection();
		}

		private void cmdDeleteItemFromList_Click(object sender, EventArgs e)
		{
			//IL_004a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0050: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)clbObservations).get_SelectedIndex();
			if (selectedIndex < 0 || (int)MessageBox.Show("Do you want to delete\r\n\r\n" + ((ObjectCollection)clbObservations.get_Items()).get_Item(selectedIndex), "Delete item", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			((ObjectCollection)clbObservations.get_Items()).RemoveAt(selectedIndex);
			if (((ObjectCollection)clbObservations.get_Items()).get_Count() > 0)
			{
				if (selectedIndex < ((ObjectCollection)clbObservations.get_Items()).get_Count())
				{
					((ListControl)clbObservations).set_SelectedIndex(selectedIndex);
				}
				else if (selectedIndex > 0)
				{
					((ListControl)clbObservations).set_SelectedIndex(selectedIndex - 1);
				}
			}
			SolveForClockCorrection();
		}

		private void cmdDuplicate_Click(object sender, EventArgs e)
		{
			//IL_0043: Unknown result type (might be due to invalid IL or missing references)
			//IL_0049: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to duplicate\r\n\r\n" + ((ObjectCollection)clbObservations.get_Items()).get_Item(((ListControl)clbObservations).get_SelectedIndex()), "Duplicate item", (MessageBoxButtons)4, (MessageBoxIcon)64, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				clbObservations.get_Items().Add((object)((ObjectCollection)clbObservations.get_Items()).get_Item(((ListControl)clbObservations).get_SelectedIndex()).ToString(), true);
				SolveForClockCorrection();
			}
		}

		private void clbObservations_MouseUp(object sender, MouseEventArgs e)
		{
			SolveForClockCorrection();
		}

		private void optLinearFit_MouseUp(object sender, MouseEventArgs e)
		{
			SolveForClockCorrection();
		}

		private void optMeanFit_MouseUp(object sender, MouseEventArgs e)
		{
			SolveForClockCorrection();
		}

		private void optRefractionNotCorrected_MouseUp(object sender, MouseEventArgs e)
		{
			SolveForClockCorrection();
		}

		private void optRefractionCorrected_MouseUp(object sender, MouseEventArgs e)
		{
			SolveForClockCorrection();
		}

		private void SolveForClockCorrection()
		{
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			double Gradient = 0.0;
			double Constant = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			double num6 = 0.0;
			double num7 = 0.0;
			double Gradient2 = 0.0;
			double Constant2 = 0.0;
			double num8 = 0.0;
			double num9 = 0.0;
			double num10 = 0.0;
			string text = "+";
			for (int i = 0; i < 2; i++)
			{
				List<PointF> list = new List<PointF>();
				num = (num2 = 0f);
				for (int j = 0; j < ((ObjectCollection)clbObservations.get_Items()).get_Count(); j++)
				{
					if (!clbObservations.GetItemChecked(j) || ((i == 0) & (((ObjectCollection)clbObservations.get_Items()).get_Item(j).ToString()!.Substring(0, 1) != NoRefractionCorrectionApplied)) || ((i == 1) & (((ObjectCollection)clbObservations.get_Items()).get_Item(j).ToString()!.Substring(0, 1) != RefractionCorrectionApplied)))
					{
						continue;
					}
					string[] array = ((ObjectCollection)clbObservations.get_Items()).get_Item(j).ToString()!.Split(new char[1] { '|' });
					PointF item = default(PointF);
					item.X = float.Parse(array[1]);
					item.Y = float.Parse(array[2]);
					num2 += item.Y;
					num += 1f;
					if (num == 1f)
					{
						if (i == 0)
						{
							num4 = (int)Math.Floor(item.X);
						}
						else
						{
							num8 = (int)Math.Floor(item.X);
						}
					}
					list.Add(item);
				}
				if (!(num > 0f))
				{
					continue;
				}
				if (i == 0)
				{
					num3 = num2 / num;
					num6 = SDofMean(list, num3);
					if (num > 1f)
					{
						num5 = FindLinearLeastSquaresFit(list, out Gradient, out Constant);
					}
				}
				else
				{
					num7 = num2 / num;
					num10 = SDofMean(list, num7);
					if (num > 1f)
					{
						num9 = FindLinearLeastSquaresFit(list, out Gradient2, out Constant2);
					}
				}
			}
			((Control)lblMean_NoRefractionCorrn).set_Text(string.Format("{0,1:f0} ±{1,1:f0} sec", num3, num6));
			((Control)lblMeanCorrectedForRefraction).set_Text(string.Format("{0,1:f0} ±{1,1:f0} sec", num7, num10));
			text = "+";
			if (Gradient < 0.0)
			{
				text = "-";
			}
			((Control)lblLinearNoRefractionCorrn).set_Text(string.Format("{0,1:f0}{1,1}{2,4:f2}*(UT-{3,1}) ±{4,1:f0} sec", Constant + num4 * Gradient, text, Math.Abs(Gradient), num4, num5));
			text = "+";
			if (Gradient2 < 0.0)
			{
				text = "-";
			}
			((Control)lblLinearCorrectedForRefraction).set_Text(string.Format("{0,1:f0}{1,1}{2,4:f2}*(UT-{3,1}) ±{4,1:f0} sec", Constant2 + num8 * Gradient2, text, Math.Abs(Gradient2), num8, num9));
			if (optMeanFit.get_Checked())
			{
				if (optRefractionCorrected.get_Checked())
				{
					ClockCorrnConstant = num7;
					ClockCorrectionRate = 0.0;
					ClockCorrectionZeroT = 0.0;
				}
				else
				{
					ClockCorrnConstant = num3;
					ClockCorrectionRate = 0.0;
					ClockCorrectionZeroT = 0.0;
				}
			}
			else if (optRefractionCorrected.get_Checked())
			{
				ClockCorrnConstant = Constant2 + num8 * Gradient2;
				ClockCorrectionRate = Gradient2;
				ClockCorrectionZeroT = num8;
			}
			else
			{
				ClockCorrnConstant = Constant + num4 * Gradient;
				ClockCorrectionRate = Gradient;
				ClockCorrectionZeroT = num4;
			}
			EventUT();
		}

		public static double FindLinearLeastSquaresFit(List<PointF> points, out double Gradient, out double Constant)
		{
			double num = points.Count;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			double num5 = 0.0;
			foreach (PointF point in points)
			{
				num2 += (double)point.X;
				num3 += (double)point.Y;
				num4 += (double)(point.X * point.X);
				num5 += (double)(point.X * point.Y);
			}
			Gradient = (num5 * num - num2 * num3) / (num4 * num - num2 * num2);
			Constant = (num5 * num2 - num3 * num4) / (num2 * num2 - num * num4);
			return Math.Sqrt(ErrorSquared(points, Gradient, Constant) / (double)points.Count);
		}

		public static double ErrorSquared(List<PointF> points, double Gradient, double Constant)
		{
			double num = 0.0;
			foreach (PointF point in points)
			{
				double num2 = (double)point.Y - (Gradient * (double)point.X + Constant);
				num += num2 * num2;
			}
			return num;
		}

		public static double SDofMean(List<PointF> points, double Mean)
		{
			double num = 0.0;
			foreach (PointF point in points)
			{
				double num2 = (double)point.Y - Mean;
				num += num2 * num2;
			}
			return Math.Sqrt(num / (double)points.Count);
		}

		private void cmdAddClockCorrnToList_Click(object sender, EventArgs e)
		{
			clbObservations.get_Items().Add((object)(ClockCorrection + " Clock corrn. Mean: " + ((Control)lblMean_NoRefractionCorrn).get_Text() + ", Linear: " + ((Control)lblLinearNoRefractionCorrn).get_Text()), false);
			clbObservations.get_Items().Add((object)(ClockCorrection + " Clock corrn. refraction adjusted Mean: " + ((Control)lblMeanCorrectedForRefraction).get_Text() + ", Linear: " + ((Control)lblLinearCorrectedForRefraction).get_Text()), false);
		}

		private void cmdAddToOutput_Click(object sender, EventArgs e)
		{
			string text = " (Mean)";
			if (optApparent.get_Checked())
			{
				text = " (Appt)";
			}
			text = ((!optMeanFit.get_Checked()) ? (text + " L") : (text + " M"));
			text = ((!optRefractionCorrected.get_Checked()) ? (text + "_") : (text + "R"));
			string text2 = ((Control)txtStar).get_Text().Trim();
			if (text2.Length < 2)
			{
				return;
			}
			string text3 = text2.Substring(0, 1);
			if ("PRSX".IndexOf(text3) >= 0)
			{
				string text4 = text2.Substring(1);
				if (text2.PadRight(1).Substring(0, 1) == "P")
				{
					text4 = text4.PadRight(4, '0');
				}
				text2 = text3 + text4.PadLeft(6, ' ');
				string text5 = ObservedEvent + ((Control)txtDay).get_Text().PadLeft(3) + " " + Utilities.DEGtoDMS(EThrs, 2, 0, MinutesOnly: false, IncludeLeadingZeros: false) + text + " = UT " + Utilities.DEGtoDMS(UTofEventHrs, 2, 0, MinutesOnly: false, IncludeLeadingZeros: false) + ", " + text2 + ", Residual = " + ((Control)lblResidual).get_Text().TrimStart(Array.Empty<char>()) + ", deltaT = " + ((Control)lbldeltaT).get_Text();
				clbObservations.get_Items().Add((object)text5, false);
			}
		}

		private void cmdAddComment_Click(object sender, EventArgs e)
		{
			//IL_0064: Unknown result type (might be due to invalid IL or missing references)
			//IL_006a: Invalid comparison between Unknown and I4
			string input = "";
			string text = "";
			bool flag = false;
			int selectedIndex = ((ListControl)clbObservations).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				text = ((ObjectCollection)clbObservations.get_Items()).get_Item(selectedIndex).ToString();
				if (text.Substring(0, 1) == CommentTag && text.Length > 2)
				{
					flag = true;
					input = text.Substring(2);
				}
			}
			if ((int)Utilities.InputDialog("Add/Edit a comment", ref input) == 1)
			{
				if (flag)
				{
					((ObjectCollection)clbObservations.get_Items()).RemoveAt(selectedIndex);
				}
				clbObservations.get_Items().Add((object)(CommentTag + " " + input), false);
			}
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(CollectList());
		}

		private string CollectList()
		{
			string text = "*" + updnYear.get_Value() + " " + cmbMonth.get_Items().get_Item(((ListControl)cmbMonth).get_SelectedIndex()).ToString() + cmbDay.get_Items().get_Item(((ListControl)cmbDay).get_SelectedIndex()).ToString()!.PadLeft(3) + "   " + cmbObserver.get_Items().get_Item(((ListControl)cmbObserver).get_SelectedIndex()).ToString() + "(" + Observers[((ListControl)cmbObserver).get_SelectedIndex()].Substring(24) + ")\r\n";
			text += "  Date & time  Type    Ref star   Altitude  Rfn   UT event   Rate     UT     Tcorrn\r\n";
			bool flag = false;
			bool flag2 = false;
			bool flag3 = false;
			bool flag4 = false;
			bool flag5 = false;
			for (int i = 0; i < ((ObjectCollection)clbObservations.get_Items()).get_Count(); i++)
			{
				if (!flag5 && ((ObjectCollection)clbObservations.get_Items()).get_Item(i).ToString()!.Substring(0, 1) == RefractionCorrectionApplied)
				{
					flag5 = true;
				}
				if (flag5 && !flag4 && ((ObjectCollection)clbObservations.get_Items()).get_Item(i).ToString()!.Substring(0, 1) == NoRefractionCorrectionApplied)
				{
					flag4 = true;
					text += "\r\n";
				}
				if (!flag && ((ObjectCollection)clbObservations.get_Items()).get_Item(i).ToString()!.Substring(0, 1) == ObservedEvent)
				{
					flag = true;
					text += "\r\n  Date & time  Type Crns      h  m  s  Star     Reduction\r\n";
				}
				if (!flag2 && ((ObjectCollection)clbObservations.get_Items()).get_Item(i).ToString()!.Substring(0, 1) == ClockCorrection)
				{
					flag2 = true;
					text += "\r\n Clock corrections\r\n";
				}
				if (!flag3 && ((ObjectCollection)clbObservations.get_Items()).get_Item(i).ToString()!.Substring(0, 1) == CommentTag)
				{
					flag3 = true;
					text += "\r\n  Comments\r\n";
				}
				text = text + ((ObjectCollection)clbObservations.get_Items()).get_Item(i).ToString() + "\r\n";
			}
			return text + "".PadRight(50, '=') + "\r\n";
		}

		private void cmdPastOutput_Click(object sender, EventArgs e)
		{
			((ObjectCollection)clbObservations.get_Items()).Clear();
			string[] array = Clipboard.GetText().Split(new char[1] { '\r' });
			for (int i = 0; i < array.Length; i++)
			{
				if (array[i].Substring(0, 1) == "\n")
				{
					array[i] = array[i].Substring(1);
				}
			}
			for (int j = 0; j < array.Length; j++)
			{
				if (array[j].Length == 0 || array[j].Substring(0, 1) == " " || array[j].Substring(0, 1) == "=" || array[j].Substring(0, 1) == "~")
				{
					continue;
				}
				if (j == 0 && array[0].Substring(0, 1) == "*")
				{
					((ObjectCollection)clbObservations.get_Items()).Clear();
					((Control)txtStar).set_Text("");
					if (!decimal.TryParse(array[0].Substring(1, 4), out var result))
					{
						result = 1600m;
					}
					updnYear.set_Value(result);
					string text = array[0].Substring(6, 3);
					for (int k = 0; k < 12; k++)
					{
						if (cmbMonth.get_Items().get_Item(k).ToString() == text)
						{
							((ListControl)cmbMonth).set_SelectedIndex(k);
						}
					}
					if (!int.TryParse(array[0].Substring(10, 2), out var result2))
					{
						result2 = 1;
					}
					((ListControl)cmbDay).set_SelectedIndex(result2);
					string text2 = array[0].Substring(14).Trim();
					int num = text2.IndexOf("(");
					if (num > 0)
					{
						text2 = text2.Substring(0, num);
					}
					for (int l = 0; l < cmbObserver.get_Items().get_Count(); l++)
					{
						if (cmbObserver.get_Items().get_Item(l).ToString()!.Contains(text2))
						{
							((ListControl)cmbObserver).set_SelectedIndex(l);
							break;
						}
					}
				}
				else
				{
					clbObservations.get_Items().Add((object)array[j], true);
				}
			}
			UpdateCalc();
			SolveForClockCorrection();
			EventUT();
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0059: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)cmbObserver).get_SelectedIndex() < 0)
			{
				MessageBox.Show("An observer has not been selected", "No observer", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			string text = cmbObserver.get_Items().get_Item(((ListControl)cmbObserver).get_SelectedIndex()).ToString();
			if (text.Length < 1)
			{
				MessageBox.Show("No valid observer has been selected", "No observer", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			int num = text.IndexOf("@");
			if (num > 0)
			{
				text = text.Substring(0, num) + "at" + text.Substring(num + 1);
			}
			text = text.Trim();
			using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Observations\\HistLog_" + text + ".txt", append: true);
			streamWriter.Write(CollectList());
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
			//IL_0360: Unknown result type (might be due to invalid IL or missing references)
			//IL_036a: Expected O, but got Unknown
			//IL_036b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0375: Expected O, but got Unknown
			//IL_0376: Unknown result type (might be due to invalid IL or missing references)
			//IL_0380: Expected O, but got Unknown
			//IL_0381: Unknown result type (might be due to invalid IL or missing references)
			//IL_038b: Expected O, but got Unknown
			//IL_038c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0396: Expected O, but got Unknown
			//IL_0397: Unknown result type (might be due to invalid IL or missing references)
			//IL_03a1: Expected O, but got Unknown
			//IL_03a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ac: Expected O, but got Unknown
			//IL_03ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_03b7: Expected O, but got Unknown
			//IL_03b8: Unknown result type (might be due to invalid IL or missing references)
			//IL_03c2: Expected O, but got Unknown
			//IL_03c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_03cd: Expected O, but got Unknown
			//IL_03ce: Unknown result type (might be due to invalid IL or missing references)
			//IL_03d8: Expected O, but got Unknown
			//IL_03d9: Unknown result type (might be due to invalid IL or missing references)
			//IL_03e3: Expected O, but got Unknown
			//IL_03e4: Unknown result type (might be due to invalid IL or missing references)
			//IL_03ee: Expected O, but got Unknown
			//IL_03ef: Unknown result type (might be due to invalid IL or missing references)
			//IL_03f9: Expected O, but got Unknown
			//IL_03fa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0404: Expected O, but got Unknown
			//IL_0405: Unknown result type (might be due to invalid IL or missing references)
			//IL_040f: Expected O, but got Unknown
			//IL_0410: Unknown result type (might be due to invalid IL or missing references)
			//IL_041a: Expected O, but got Unknown
			//IL_041b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0425: Expected O, but got Unknown
			//IL_0426: Unknown result type (might be due to invalid IL or missing references)
			//IL_0430: Expected O, but got Unknown
			//IL_0431: Unknown result type (might be due to invalid IL or missing references)
			//IL_043b: Expected O, but got Unknown
			//IL_043c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0446: Expected O, but got Unknown
			//IL_0447: Unknown result type (might be due to invalid IL or missing references)
			//IL_0451: Expected O, but got Unknown
			//IL_0452: Unknown result type (might be due to invalid IL or missing references)
			//IL_045c: Expected O, but got Unknown
			//IL_045d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0467: Expected O, but got Unknown
			//IL_0468: Unknown result type (might be due to invalid IL or missing references)
			//IL_0472: Expected O, but got Unknown
			//IL_0473: Unknown result type (might be due to invalid IL or missing references)
			//IL_047d: Expected O, but got Unknown
			//IL_047e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0488: Expected O, but got Unknown
			//IL_0489: Unknown result type (might be due to invalid IL or missing references)
			//IL_0493: Expected O, but got Unknown
			//IL_0494: Unknown result type (might be due to invalid IL or missing references)
			//IL_049e: Expected O, but got Unknown
			//IL_049f: Unknown result type (might be due to invalid IL or missing references)
			//IL_04a9: Expected O, but got Unknown
			//IL_04aa: Unknown result type (might be due to invalid IL or missing references)
			//IL_04b4: Expected O, but got Unknown
			//IL_04b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_04bf: Expected O, but got Unknown
			//IL_04c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_04ca: Expected O, but got Unknown
			//IL_04cb: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d5: Expected O, but got Unknown
			//IL_04d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_04e0: Expected O, but got Unknown
			//IL_04e1: Unknown result type (might be due to invalid IL or missing references)
			//IL_04eb: Expected O, but got Unknown
			//IL_04ec: Unknown result type (might be due to invalid IL or missing references)
			//IL_04f6: Expected O, but got Unknown
			//IL_04f7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0501: Expected O, but got Unknown
			//IL_0502: Unknown result type (might be due to invalid IL or missing references)
			//IL_050c: Expected O, but got Unknown
			//IL_050d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0517: Expected O, but got Unknown
			//IL_0518: Unknown result type (might be due to invalid IL or missing references)
			//IL_0522: Expected O, but got Unknown
			//IL_0523: Unknown result type (might be due to invalid IL or missing references)
			//IL_052d: Expected O, but got Unknown
			//IL_052e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0538: Expected O, but got Unknown
			//IL_0539: Unknown result type (might be due to invalid IL or missing references)
			//IL_0543: Expected O, but got Unknown
			//IL_0544: Unknown result type (might be due to invalid IL or missing references)
			//IL_054e: Expected O, but got Unknown
			//IL_054f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0559: Expected O, but got Unknown
			//IL_055a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0564: Expected O, but got Unknown
			//IL_0565: Unknown result type (might be due to invalid IL or missing references)
			//IL_056f: Expected O, but got Unknown
			//IL_0570: Unknown result type (might be due to invalid IL or missing references)
			//IL_057a: Expected O, but got Unknown
			//IL_057b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0585: Expected O, but got Unknown
			//IL_0586: Unknown result type (might be due to invalid IL or missing references)
			//IL_0590: Expected O, but got Unknown
			//IL_0591: Unknown result type (might be due to invalid IL or missing references)
			//IL_059b: Expected O, but got Unknown
			//IL_059c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05a6: Expected O, but got Unknown
			//IL_05a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_05b1: Expected O, but got Unknown
			//IL_05b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_05bc: Expected O, but got Unknown
			//IL_05bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_05c7: Expected O, but got Unknown
			//IL_05c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_05d2: Expected O, but got Unknown
			//IL_05d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_05dd: Expected O, but got Unknown
			//IL_05de: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e8: Expected O, but got Unknown
			//IL_05e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_05f3: Expected O, but got Unknown
			//IL_05f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_05fe: Expected O, but got Unknown
			//IL_05ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_0609: Expected O, but got Unknown
			//IL_060a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0614: Expected O, but got Unknown
			//IL_0615: Unknown result type (might be due to invalid IL or missing references)
			//IL_061f: Expected O, but got Unknown
			//IL_0620: Unknown result type (might be due to invalid IL or missing references)
			//IL_062a: Expected O, but got Unknown
			//IL_062b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0635: Expected O, but got Unknown
			//IL_0636: Unknown result type (might be due to invalid IL or missing references)
			//IL_0640: Expected O, but got Unknown
			//IL_0641: Unknown result type (might be due to invalid IL or missing references)
			//IL_064b: Expected O, but got Unknown
			//IL_099f: Unknown result type (might be due to invalid IL or missing references)
			//IL_09a9: Expected O, but got Unknown
			//IL_0a31: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a3b: Expected O, but got Unknown
			//IL_2c9e: Unknown result type (might be due to invalid IL or missing references)
			//IL_2ca8: Expected O, but got Unknown
			//IL_2d21: Unknown result type (might be due to invalid IL or missing references)
			//IL_2d2b: Expected O, but got Unknown
			//IL_2ece: Unknown result type (might be due to invalid IL or missing references)
			//IL_2ed8: Expected O, but got Unknown
			//IL_2f51: Unknown result type (might be due to invalid IL or missing references)
			//IL_2f5b: Expected O, but got Unknown
			//IL_50e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_50f3: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(EventTimeFromStarAltitude));
			cmbObserver = new ComboBox();
			label1 = new Label();
			cmbStarForAltitude = new ComboBox();
			label3 = new Label();
			label4 = new Label();
			txtStarAltDeg = new TextBox();
			txtStarAltMin = new TextBox();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			cmbDay = new ComboBox();
			cmbMonth = new ComboBox();
			updnYear = new NumericUpDown();
			label11 = new Label();
			label12 = new Label();
			label13 = new Label();
			txtLatS = new TextBox();
			txtLatM = new TextBox();
			txtLatD = new TextBox();
			txtLongS = new TextBox();
			txtLongM = new TextBox();
			txtLongD = new TextBox();
			txtAltitude = new TextBox();
			label24 = new Label();
			label25 = new Label();
			label20 = new Label();
			label10 = new Label();
			label14 = new Label();
			label15 = new Label();
			label16 = new Label();
			txtStarCoords = new TextBox();
			label17 = new Label();
			lblUTTransit = new Label();
			panelAlt = new Panel();
			label37 = new Label();
			lblCorrectedAltitude = new Label();
			lblRefraction = new Label();
			label36 = new Label();
			label38 = new Label();
			chkCorrectForRefraction = new CheckBox();
			label19 = new Label();
			label21 = new Label();
			label22 = new Label();
			lblUTSet = new Label();
			lblUTRise = new Label();
			label18 = new Label();
			label23 = new Label();
			lblMeanRise = new Label();
			lblMeanSet = new Label();
			lblMeanTransit = new Label();
			label29 = new Label();
			lblApparentRise = new Label();
			lblApparentSet = new Label();
			lblApparentTransit = new Label();
			txtS = new TextBox();
			txtM = new TextBox();
			txtH = new TextBox();
			label26 = new Label();
			label27 = new Label();
			label28 = new Label();
			panel1 = new Panel();
			label62 = new Label();
			lblClockCorrection = new Label();
			label33 = new Label();
			label34 = new Label();
			cmdAddToOutput = new Button();
			panel6 = new Panel();
			label59 = new Label();
			optRefractionCorrected = new RadioButton();
			optRefractionNotCorrected = new RadioButton();
			panel7 = new Panel();
			label60 = new Label();
			optLinearFit = new RadioButton();
			optMeanFit = new RadioButton();
			lblEventCorrectedUT = new Label();
			label48 = new Label();
			label49 = new Label();
			label50 = new Label();
			label51 = new Label();
			lblResidual = new Label();
			label43 = new Label();
			label42 = new Label();
			txtStar = new TextBox();
			label35 = new Label();
			label32 = new Label();
			label52 = new Label();
			label53 = new Label();
			label54 = new Label();
			label55 = new Label();
			txtDay = new TextBox();
			label44 = new Label();
			label2 = new Label();
			label30 = new Label();
			lblEventUT = new Label();
			optApparent = new RadioButton();
			optMean = new RadioButton();
			label31 = new Label();
			lbldT_RiseSet = new Label();
			menuStrip1 = new MenuStrip();
			resetDateObservationsToolStripMenuItem = new ToolStripMenuItem();
			downloadUpdatesToolStripMenuItem = new ToolStripMenuItem();
			starNamesToolStripMenuItem = new ToolStripMenuItem();
			observersToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			label39 = new Label();
			label40 = new Label();
			label41 = new Label();
			panel2 = new Panel();
			cmdTFApparentSet = new Button();
			cmdTFApparentRise = new Button();
			cmdTFApparentTransit = new Button();
			cmdTFMeanSet = new Button();
			cmdTFMeanRise = new Button();
			cmdTFMeanTransit = new Button();
			pnlSiteCoords = new Panel();
			panel3 = new Panel();
			panel4 = new Panel();
			cmdAddComment = new Button();
			label61 = new Label();
			cmdSave = new Button();
			cmdPastOutput = new Button();
			cmdCopy = new Button();
			cmdAddClockCorrnToList = new Button();
			label58 = new Label();
			label57 = new Label();
			label56 = new Label();
			lblMeanCorrectedForRefraction = new Label();
			lblLinearCorrectedForRefraction = new Label();
			clbObservations = new CheckedListBox();
			lblLinearNoRefractionCorrn = new Label();
			label47 = new Label();
			lblMean_NoRefractionCorrn = new Label();
			label45 = new Label();
			cmdDuplicate = new Button();
			cmdDeleteItemFromList = new Button();
			label46 = new Label();
			cmdAddRise = new Button();
			cmdAddSet = new Button();
			cmdAddTransit = new Button();
			panel5 = new Panel();
			label63 = new Label();
			lbldeltaT = new Label();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)panelAlt).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panel6).SuspendLayout();
			((Control)panel7).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)pnlSiteCoords).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)panel5).SuspendLayout();
			((Control)this).SuspendLayout();
			((ListControl)cmbObserver).set_FormattingEnabled(true);
			((Control)cmbObserver).set_Location(new Point(5, 26));
			((Control)cmbObserver).set_Name("cmbObserver");
			((Control)cmbObserver).set_Size(new Size(221, 21));
			((Control)cmbObserver).set_TabIndex(1);
			cmbObserver.add_SelectedIndexChanged((EventHandler)cmbObserver_SelectedIndexChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(4, 11));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(118, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Observer && location");
			((ListControl)cmbStarForAltitude).set_FormattingEnabled(true);
			((Control)cmbStarForAltitude).set_Location(new Point(4, 24));
			((Control)cmbStarForAltitude).set_Name("cmbStarForAltitude");
			((Control)cmbStarForAltitude).set_Size(new Size(221, 21));
			((Control)cmbStarForAltitude).set_TabIndex(1);
			cmbStarForAltitude.add_SelectedIndexChanged((EventHandler)cmbStarForAltitude_SelectedIndexChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(3, 9));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(210, 13));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("Altitude measurement - name of star");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(187, 63));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(90, 13));
			((Control)label4).set_TabIndex(5);
			((Control)label4).set_Text("Altitude of star");
			((Control)txtStarAltDeg).set_Location(new Point(201, 90));
			((TextBoxBase)txtStarAltDeg).set_MaxLength(2);
			((Control)txtStarAltDeg).set_Name("txtStarAltDeg");
			((Control)txtStarAltDeg).set_Size(new Size(29, 20));
			((Control)txtStarAltDeg).set_TabIndex(13);
			((Control)txtStarAltDeg).set_Text("30");
			((Control)txtStarAltDeg).add_TextChanged((EventHandler)txtStarAltDeg_TextChanged);
			((Control)txtStarAltDeg).add_MouseUp(new MouseEventHandler(txtStarAltDeg_MouseUp));
			((Control)txtStarAltMin).set_Location(new Point(234, 90));
			((TextBoxBase)txtStarAltMin).set_MaxLength(4);
			((Control)txtStarAltMin).set_Name("txtStarAltMin");
			((Control)txtStarAltMin).set_Size(new Size(26, 20));
			((Control)txtStarAltMin).set_TabIndex(15);
			((Control)txtStarAltMin).set_Text("0.0");
			((Control)txtStarAltMin).add_TextChanged((EventHandler)txtStarAltMin_TextChanged);
			((Control)txtStarAltMin).add_MouseUp(new MouseEventHandler(txtStarAltMin_MouseUp));
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(218, 77));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(13, 13));
			((Control)label5).set_TabIndex(12);
			((Control)label5).set_Text("o");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(243, 82));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(9, 13));
			((Control)label6).set_TabIndex(14);
			((Control)label6).set_Text("'");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(116, 76));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(26, 13));
			((Control)label7).set_TabIndex(10);
			((Control)label7).set_Text("Day");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(67, 76));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(8);
			((Control)label8).set_Text("Month");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(21, 76));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(29, 13));
			((Control)label9).set_TabIndex(6);
			((Control)label9).set_Text("Year");
			cmbDay.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbDay).set_FormattingEnabled(true);
			cmbDay.get_Items().AddRange(new object[32]
			{
				"0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
				"10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
				"20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
				"30", "31"
			});
			((Control)cmbDay).set_Location(new Point(114, 91));
			cmbDay.set_MaxDropDownItems(31);
			((Control)cmbDay).set_Name("cmbDay");
			((Control)cmbDay).set_Size(new Size(40, 21));
			((Control)cmbDay).set_TabIndex(11);
			cmbDay.add_SelectedIndexChanged((EventHandler)cmbDay_SelectedIndexChanged);
			cmbMonth.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbMonth).set_FormattingEnabled(true);
			cmbMonth.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbMonth).set_Location(new Point(71, 91));
			cmbMonth.set_MaxDropDownItems(12);
			((Control)cmbMonth).set_Name("cmbMonth");
			((Control)cmbMonth).set_Size(new Size(43, 21));
			((Control)cmbMonth).set_TabIndex(9);
			cmbMonth.add_SelectedIndexChanged((EventHandler)cmbMonth_SelectedIndexChanged);
			((Control)updnYear).set_Location(new Point(20, 91));
			updnYear.set_Maximum(new decimal(new int[4] { 17000, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 13000, 0, 0, -2147483648 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(51, 20));
			((Control)updnYear).set_TabIndex(7);
			updnYear.set_Value(new decimal(new int[4] { 1675, 0, 0, 0 }));
			updnYear.add_ValueChanged((EventHandler)updnYear_ValueChanged);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(207, 1));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(42, 13));
			((Control)label11).set_TabIndex(2);
			((Control)label11).set_Text("Altitude");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(113, 1));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(45, 13));
			((Control)label12).set_TabIndex(1);
			((Control)label12).set_Text("Latitude");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(12, 1));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(67, 13));
			((Control)label13).set_TabIndex(0);
			((Control)label13).set_Text("E. Longitude");
			((Control)txtLatS).set_Location(new Point(157, 23));
			((Control)txtLatS).set_Name("txtLatS");
			((Control)txtLatS).set_Size(new Size(31, 20));
			((Control)txtLatS).set_TabIndex(15);
			((Control)txtLatS).set_Text("0");
			txtLatS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLatM).set_Location(new Point(136, 23));
			((Control)txtLatM).set_Name("txtLatM");
			((Control)txtLatM).set_Size(new Size(20, 20));
			((Control)txtLatM).set_TabIndex(14);
			((Control)txtLatM).set_Text("0");
			txtLatM.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLatD).set_Location(new Point(107, 23));
			((Control)txtLatD).set_Name("txtLatD");
			((Control)txtLatD).set_Size(new Size(28, 20));
			((Control)txtLatD).set_TabIndex(13);
			((Control)txtLatD).set_Text("0");
			txtLatD.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongS).set_Location(new Point(58, 23));
			((Control)txtLongS).set_Name("txtLongS");
			((Control)txtLongS).set_Size(new Size(31, 20));
			((Control)txtLongS).set_TabIndex(12);
			((Control)txtLongS).set_Text("0");
			txtLongS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLongM).set_Location(new Point(37, 23));
			((Control)txtLongM).set_Name("txtLongM");
			((Control)txtLongM).set_Size(new Size(20, 20));
			((Control)txtLongM).set_TabIndex(11);
			((Control)txtLongM).set_Text("0");
			txtLongM.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongD).set_Location(new Point(8, 23));
			((Control)txtLongD).set_Name("txtLongD");
			((Control)txtLongD).set_Size(new Size(28, 20));
			((Control)txtLongD).set_TabIndex(10);
			((Control)txtLongD).set_Text("0");
			txtLongD.set_TextAlign((HorizontalAlignment)1);
			((Control)txtAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAltitude).set_Location(new Point(210, 23));
			((Control)txtAltitude).set_Name("txtAltitude");
			((Control)txtAltitude).set_Size(new Size(38, 20));
			((Control)txtAltitude).set_TabIndex(16);
			((Control)txtAltitude).set_Text("0");
			txtAltitude.set_TextAlign((HorizontalAlignment)2);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(45, 15));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(9, 13));
			((Control)label24).set_TabIndex(4);
			((Control)label24).set_Text("'");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(71, 15));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(12, 13));
			((Control)label25).set_TabIndex(5);
			((Control)label25).set_Text("\"");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(24, 11));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(13, 13));
			((Control)label20).set_TabIndex(3);
			((Control)label20).set_Text("o");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(144, 14));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(9, 13));
			((Control)label10).set_TabIndex(7);
			((Control)label10).set_Text("'");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(170, 14));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(12, 13));
			((Control)label14).set_TabIndex(8);
			((Control)label14).set_Text("\"");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(123, 10));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(13, 13));
			((Control)label15).set_TabIndex(6);
			((Control)label15).set_Text("o");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(222, 10));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(15, 13));
			((Control)label16).set_TabIndex(0);
			((Control)label16).set_Text("m");
			((Control)txtStarCoords).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStarCoords).set_Location(new Point(275, 17));
			((TextBoxBase)txtStarCoords).set_Multiline(true);
			((Control)txtStarCoords).set_Name("txtStarCoords");
			((TextBoxBase)txtStarCoords).set_ReadOnly(true);
			((Control)txtStarCoords).set_Size(new Size(274, 36));
			((Control)txtStarCoords).set_TabIndex(3);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(279, 2));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(84, 13));
			((Control)label17).set_TabIndex(2);
			((Control)label17).set_Text("Star coordinates");
			((Control)lblUTTransit).set_AutoSize(true);
			((Control)lblUTTransit).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUTTransit).set_Location(new Point(86, 32));
			((Control)lblUTTransit).set_Name("lblUTTransit");
			((Control)lblUTTransit).set_Size(new Size(91, 14));
			((Control)lblUTTransit).set_TabIndex(5);
			((Control)lblUTTransit).set_Text("00  00 00 00");
			panelAlt.set_BorderStyle((BorderStyle)1);
			((Control)panelAlt).get_Controls().Add((Control)(object)label37);
			((Control)panelAlt).get_Controls().Add((Control)(object)lblCorrectedAltitude);
			((Control)panelAlt).get_Controls().Add((Control)(object)lblRefraction);
			((Control)panelAlt).get_Controls().Add((Control)(object)label36);
			((Control)panelAlt).get_Controls().Add((Control)(object)txtStarAltMin);
			((Control)panelAlt).get_Controls().Add((Control)(object)label38);
			((Control)panelAlt).get_Controls().Add((Control)(object)label17);
			((Control)panelAlt).get_Controls().Add((Control)(object)txtStarAltDeg);
			((Control)panelAlt).get_Controls().Add((Control)(object)label7);
			((Control)panelAlt).get_Controls().Add((Control)(object)txtStarCoords);
			((Control)panelAlt).get_Controls().Add((Control)(object)label8);
			((Control)panelAlt).get_Controls().Add((Control)(object)label4);
			((Control)panelAlt).get_Controls().Add((Control)(object)label9);
			((Control)panelAlt).get_Controls().Add((Control)(object)label5);
			((Control)panelAlt).get_Controls().Add((Control)(object)cmbDay);
			((Control)panelAlt).get_Controls().Add((Control)(object)label6);
			((Control)panelAlt).get_Controls().Add((Control)(object)cmbMonth);
			((Control)panelAlt).get_Controls().Add((Control)(object)chkCorrectForRefraction);
			((Control)panelAlt).get_Controls().Add((Control)(object)updnYear);
			((Control)panelAlt).get_Controls().Add((Control)(object)label3);
			((Control)panelAlt).get_Controls().Add((Control)(object)cmbStarForAltitude);
			((Control)panelAlt).set_Location(new Point(17, 123));
			((Control)panelAlt).set_Name("panelAlt");
			((Control)panelAlt).set_Size(new Size(566, 124));
			((Control)panelAlt).set_TabIndex(9);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(467, 63));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(79, 13));
			((Control)label37).set_TabIndex(19);
			((Control)label37).set_Text("True altitude");
			((Control)lblCorrectedAltitude).set_AutoSize(true);
			((Control)lblCorrectedAltitude).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)lblCorrectedAltitude).set_Location(new Point(479, 90));
			((Control)lblCorrectedAltitude).set_Name("lblCorrectedAltitude");
			((Control)lblCorrectedAltitude).set_Size(new Size(43, 13));
			((Control)lblCorrectedAltitude).set_TabIndex(20);
			((Control)lblCorrectedAltitude).set_Text("00 00.0");
			((Control)lblRefraction).set_AutoSize(true);
			((Control)lblRefraction).set_Location(new Point(341, 90));
			((Control)lblRefraction).set_Name("lblRefraction");
			((Control)lblRefraction).set_Size(new Size(24, 13));
			((Control)lblRefraction).set_TabIndex(17);
			((Control)lblRefraction).set_Text("0.0'");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(320, 63));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(66, 13));
			((Control)label36).set_TabIndex(16);
			((Control)label36).set_Text("Refraction");
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(2, 63));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(171, 13));
			((Control)label38).set_TabIndex(4);
			((Control)label38).set_Text("UT date of Greenwich transit");
			((Control)chkCorrectForRefraction).set_AutoSize(true);
			chkCorrectForRefraction.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)chkCorrectForRefraction).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkCorrectForRefraction).set_Location(new Point(400, 63));
			((Control)chkCorrectForRefraction).set_Name("chkCorrectForRefraction");
			((Control)chkCorrectForRefraction).set_Size(new Size(53, 42));
			((Control)chkCorrectForRefraction).set_TabIndex(18);
			((Control)chkCorrectForRefraction).set_Text("Correct for\r\nrefraction");
			((ButtonBase)chkCorrectForRefraction).set_UseVisualStyleBackColor(true);
			chkCorrectForRefraction.add_CheckedChanged((EventHandler)chkCorrectForRefraction_CheckedChanged);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Location(new Point(3, 32));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(64, 13));
			((Control)label19).set_TabIndex(0);
			((Control)label19).set_Text("Local transit");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(3, 47));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(57, 13));
			((Control)label21).set_TabIndex(1);
			((Control)label21).set_Text("Local Rise");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(3, 62));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(52, 13));
			((Control)label22).set_TabIndex(2);
			((Control)label22).set_Text("Local Set");
			((Control)lblUTSet).set_AutoSize(true);
			((Control)lblUTSet).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUTSet).set_Location(new Point(86, 62));
			((Control)lblUTSet).set_Name("lblUTSet");
			((Control)lblUTSet).set_Size(new Size(91, 14));
			((Control)lblUTSet).set_TabIndex(7);
			((Control)lblUTSet).set_Text("00  00 00 00");
			((Control)lblUTRise).set_AutoSize(true);
			((Control)lblUTRise).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUTRise).set_Location(new Point(86, 47));
			((Control)lblUTRise).set_Name("lblUTRise");
			((Control)lblUTRise).set_Size(new Size(91, 14));
			((Control)lblUTRise).set_TabIndex(6);
			((Control)lblUTRise).set_Text("00  00 00 00");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(120, 4));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(22, 13));
			((Control)label18).set_TabIndex(3);
			((Control)label18).set_Text("UT");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(233, 4));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(60, 13));
			((Control)label23).set_TabIndex(8);
			((Control)label23).set_Text("Mean Time");
			((Control)lblMeanRise).set_AutoSize(true);
			((Control)lblMeanRise).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMeanRise).set_Location(new Point(218, 47));
			((Control)lblMeanRise).set_Name("lblMeanRise");
			((Control)lblMeanRise).set_Size(new Size(91, 14));
			((Control)lblMeanRise).set_TabIndex(11);
			((Control)lblMeanRise).set_Text("00  00 00 00");
			((Control)lblMeanSet).set_AutoSize(true);
			((Control)lblMeanSet).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMeanSet).set_Location(new Point(218, 62));
			((Control)lblMeanSet).set_Name("lblMeanSet");
			((Control)lblMeanSet).set_Size(new Size(91, 14));
			((Control)lblMeanSet).set_TabIndex(12);
			((Control)lblMeanSet).set_Text("00  00 00 00");
			((Control)lblMeanTransit).set_AutoSize(true);
			((Control)lblMeanTransit).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblMeanTransit).set_Location(new Point(218, 32));
			((Control)lblMeanTransit).set_Name("lblMeanTransit");
			((Control)lblMeanTransit).set_Size(new Size(91, 14));
			((Control)lblMeanTransit).set_TabIndex(10);
			((Control)lblMeanTransit).set_Text("00  00 00 00");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Location(new Point(348, 4));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(72, 13));
			((Control)label29).set_TabIndex(0);
			((Control)label29).set_Text("Apparent time");
			((Control)lblApparentRise).set_AutoSize(true);
			((Control)lblApparentRise).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblApparentRise).set_Location(new Point(339, 47));
			((Control)lblApparentRise).set_Name("lblApparentRise");
			((Control)lblApparentRise).set_Size(new Size(91, 14));
			((Control)lblApparentRise).set_TabIndex(16);
			((Control)lblApparentRise).set_Text("00  00 00 00");
			((Control)lblApparentSet).set_AutoSize(true);
			((Control)lblApparentSet).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblApparentSet).set_Location(new Point(339, 62));
			((Control)lblApparentSet).set_Name("lblApparentSet");
			((Control)lblApparentSet).set_Size(new Size(91, 14));
			((Control)lblApparentSet).set_TabIndex(17);
			((Control)lblApparentSet).set_Text("00  00 00 00");
			((Control)lblApparentTransit).set_AutoSize(true);
			((Control)lblApparentTransit).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblApparentTransit).set_Location(new Point(339, 32));
			((Control)lblApparentTransit).set_Name("lblApparentTransit");
			((Control)lblApparentTransit).set_Size(new Size(91, 14));
			((Control)lblApparentTransit).set_TabIndex(15);
			((Control)lblApparentTransit).set_Text("00  00 00 00");
			((Control)txtS).set_Location(new Point(70, 28));
			((TextBoxBase)txtS).set_MaxLength(2);
			((Control)txtS).set_Name("txtS");
			((Control)txtS).set_Size(new Size(31, 20));
			((Control)txtS).set_TabIndex(8);
			((Control)txtS).set_Text("0");
			txtS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtS).add_TextChanged((EventHandler)txtS_TextChanged);
			((Control)txtM).set_Location(new Point(49, 28));
			((TextBoxBase)txtM).set_MaxLength(2);
			((Control)txtM).set_Name("txtM");
			((Control)txtM).set_Size(new Size(20, 20));
			((Control)txtM).set_TabIndex(7);
			((Control)txtM).set_Text("0");
			txtM.set_TextAlign((HorizontalAlignment)1);
			((Control)txtM).add_TextChanged((EventHandler)txtM_TextChanged);
			((Control)txtH).set_Location(new Point(28, 28));
			((TextBoxBase)txtH).set_MaxLength(2);
			((Control)txtH).set_Name("txtH");
			((Control)txtH).set_Size(new Size(20, 20));
			((Control)txtH).set_TabIndex(6);
			((Control)txtH).set_Text("0");
			txtH.set_TextAlign((HorizontalAlignment)1);
			((Control)txtH).add_TextChanged((EventHandler)txtH_TextChanged);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Location(new Point(54, 15));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(15, 13));
			((Control)label26).set_TabIndex(2);
			((Control)label26).set_Text("m");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Location(new Point(80, 15));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(12, 13));
			((Control)label27).set_TabIndex(3);
			((Control)label27).set_Text("s");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(33, 15));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(13, 13));
			((Control)label28).set_TabIndex(149);
			((Control)label28).set_Text("h");
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)lbldeltaT);
			((Control)panel1).get_Controls().Add((Control)(object)label63);
			((Control)panel1).get_Controls().Add((Control)(object)label62);
			((Control)panel1).get_Controls().Add((Control)(object)lblClockCorrection);
			((Control)panel1).get_Controls().Add((Control)(object)label33);
			((Control)panel1).get_Controls().Add((Control)(object)label34);
			((Control)panel1).get_Controls().Add((Control)(object)cmdAddToOutput);
			((Control)panel1).get_Controls().Add((Control)(object)panel6);
			((Control)panel1).get_Controls().Add((Control)(object)panel7);
			((Control)panel1).get_Controls().Add((Control)(object)lblEventCorrectedUT);
			((Control)panel1).get_Controls().Add((Control)(object)label48);
			((Control)panel1).get_Controls().Add((Control)(object)label49);
			((Control)panel1).get_Controls().Add((Control)(object)label50);
			((Control)panel1).get_Controls().Add((Control)(object)label51);
			((Control)panel1).get_Controls().Add((Control)(object)lblResidual);
			((Control)panel1).get_Controls().Add((Control)(object)label43);
			((Control)panel1).get_Controls().Add((Control)(object)label42);
			((Control)panel1).get_Controls().Add((Control)(object)txtStar);
			((Control)panel1).get_Controls().Add((Control)(object)label35);
			((Control)panel1).get_Controls().Add((Control)(object)label32);
			((Control)panel1).set_Location(new Point(604, 129));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(166, 228));
			((Control)panel1).set_TabIndex(12);
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(6, 147));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(83, 13));
			((Control)label62).set_TabIndex(180);
			((Control)label62).set_Text("Star identifier");
			((Control)lblClockCorrection).set_AutoSize(true);
			((Control)lblClockCorrection).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)lblClockCorrection).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblClockCorrection).set_Location(new Point(118, 87));
			((Control)lblClockCorrection).set_Name("lblClockCorrection");
			((Control)lblClockCorrection).set_Size(new Size(43, 13));
			((Control)lblClockCorrection).set_TabIndex(179);
			((Control)lblClockCorrection).set_Text(" 00 00");
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(129, 73));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(13, 13));
			((Control)label33).set_TabIndex(176);
			((Control)label33).set_Text("m");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(147, 73));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(13, 13));
			((Control)label34).set_TabIndex(177);
			((Control)label34).set_Text("s");
			((Control)cmdAddToOutput).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)cmdAddToOutput).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddToOutput).set_Location(new Point(82, 193));
			((Control)cmdAddToOutput).set_Name("cmdAddToOutput");
			((Control)cmdAddToOutput).set_Size(new Size(77, 29));
			((Control)cmdAddToOutput).set_TabIndex(175);
			((Control)cmdAddToOutput).set_Text("Add event");
			((ButtonBase)cmdAddToOutput).set_UseVisualStyleBackColor(false);
			((Control)cmdAddToOutput).add_Click((EventHandler)cmdAddToOutput_Click);
			panel6.set_BorderStyle((BorderStyle)1);
			((Control)panel6).get_Controls().Add((Control)(object)label59);
			((Control)panel6).get_Controls().Add((Control)(object)optRefractionCorrected);
			((Control)panel6).get_Controls().Add((Control)(object)optRefractionNotCorrected);
			((Control)panel6).set_Location(new Point(9, 4));
			((Control)panel6).set_Name("panel6");
			((Control)panel6).set_Size(new Size(146, 34));
			((Control)panel6).set_TabIndex(18);
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label59).set_Location(new Point(4, 0));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(136, 13));
			((Control)label59).set_TabIndex(2);
			((Control)label59).set_Text("Apply refraction to Alts");
			((Control)optRefractionCorrected).set_AutoSize(true);
			optRefractionCorrected.set_Checked(true);
			((Control)optRefractionCorrected).set_Location(new Point(27, 13));
			((Control)optRefractionCorrected).set_Name("optRefractionCorrected");
			((Control)optRefractionCorrected).set_Size(new Size(43, 17));
			((Control)optRefractionCorrected).set_TabIndex(1);
			optRefractionCorrected.set_TabStop(true);
			((Control)optRefractionCorrected).set_Text("Yes");
			((ButtonBase)optRefractionCorrected).set_UseVisualStyleBackColor(true);
			((Control)optRefractionCorrected).add_MouseUp(new MouseEventHandler(optRefractionCorrected_MouseUp));
			((Control)optRefractionNotCorrected).set_AutoSize(true);
			((Control)optRefractionNotCorrected).set_Location(new Point(78, 13));
			((Control)optRefractionNotCorrected).set_Name("optRefractionNotCorrected");
			((Control)optRefractionNotCorrected).set_Size(new Size(39, 17));
			((Control)optRefractionNotCorrected).set_TabIndex(0);
			((Control)optRefractionNotCorrected).set_Text("No");
			((ButtonBase)optRefractionNotCorrected).set_UseVisualStyleBackColor(true);
			((Control)optRefractionNotCorrected).add_MouseUp(new MouseEventHandler(optRefractionNotCorrected_MouseUp));
			panel7.set_BorderStyle((BorderStyle)1);
			((Control)panel7).get_Controls().Add((Control)(object)label60);
			((Control)panel7).get_Controls().Add((Control)(object)optLinearFit);
			((Control)panel7).get_Controls().Add((Control)(object)optMeanFit);
			((Control)panel7).set_Location(new Point(9, 40));
			((Control)panel7).set_Name("panel7");
			((Control)panel7).set_Size(new Size(146, 31));
			((Control)panel7).set_TabIndex(19);
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label60).set_Location(new Point(22, 0));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(101, 13));
			((Control)label60).set_TabIndex(3);
			((Control)label60).set_Text("Clock Correction");
			((Control)optLinearFit).set_AutoSize(true);
			optLinearFit.set_Checked(true);
			((Control)optLinearFit).set_Location(new Point(68, 12));
			((Control)optLinearFit).set_Name("optLinearFit");
			((Control)optLinearFit).set_Size(new Size(65, 17));
			((Control)optLinearFit).set_TabIndex(1);
			optLinearFit.set_TabStop(true);
			((Control)optLinearFit).set_Text("Linear fit");
			((ButtonBase)optLinearFit).set_UseVisualStyleBackColor(true);
			((Control)optLinearFit).add_MouseUp(new MouseEventHandler(optLinearFit_MouseUp));
			((Control)optMeanFit).set_AutoSize(true);
			((Control)optMeanFit).set_Location(new Point(12, 12));
			((Control)optMeanFit).set_Name("optMeanFit");
			((Control)optMeanFit).set_Size(new Size(52, 17));
			((Control)optMeanFit).set_TabIndex(0);
			((Control)optMeanFit).set_Text("Mean");
			((ButtonBase)optMeanFit).set_UseVisualStyleBackColor(true);
			((Control)optMeanFit).add_MouseUp(new MouseEventHandler(optMeanFit_MouseUp));
			((Control)lblEventCorrectedUT).set_AutoSize(true);
			((Control)lblEventCorrectedUT).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)lblEventCorrectedUT).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEventCorrectedUT).set_Location(new Point(82, 114));
			((Control)lblEventCorrectedUT).set_Name("lblEventCorrectedUT");
			((Control)lblEventCorrectedUT).set_Size(new Size(79, 13));
			((Control)lblEventCorrectedUT).set_TabIndex(162);
			((Control)lblEventCorrectedUT).set_Text("00  00 00 00");
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(88, 104));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(13, 13));
			((Control)label48).set_TabIndex(171);
			((Control)label48).set_Text("d");
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(129, 104));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(13, 13));
			((Control)label49).set_TabIndex(172);
			((Control)label49).set_Text("m");
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(147, 104));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(13, 13));
			((Control)label50).set_TabIndex(173);
			((Control)label50).set_Text("s");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label51).set_Location(new Point(111, 104));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(13, 13));
			((Control)label51).set_TabIndex(174);
			((Control)label51).set_Text("h");
			((Control)lblResidual).set_AutoSize(true);
			((Control)lblResidual).set_Location(new Point(6, 177));
			((Control)lblResidual).set_Name("lblResidual");
			((Control)lblResidual).set_Size(new Size(28, 13));
			((Control)lblResidual).set_TabIndex(167);
			((Control)lblResidual).set_Text("0.00");
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Location(new Point(6, 164));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(59, 13));
			((Control)label43).set_TabIndex(166);
			((Control)label43).set_Text("Reduction ");
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(73, 129));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(86, 13));
			((Control)label42).set_TabIndex(165);
			((Control)label42).set_Text("Rn, Sn, Xn or Pn");
			((Control)txtStar).set_Location(new Point(92, 144));
			((TextBoxBase)txtStar).set_MaxLength(7);
			((Control)txtStar).set_Name("txtStar");
			((Control)txtStar).set_Size(new Size(64, 20));
			((Control)txtStar).set_TabIndex(13);
			((Control)txtStar).add_TextChanged((EventHandler)txtStar_TextChanged);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Location(new Point(6, 114));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(71, 13));
			((Control)label35).set_TabIndex(163);
			((Control)label35).set_Text("Corrected UT");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Location(new Point(6, 86));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(84, 13));
			((Control)label32).set_TabIndex(157);
			((Control)label32).set_Text("Clock correction");
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label52).set_Location(new Point(90, 54));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(13, 13));
			((Control)label52).set_TabIndex(175);
			((Control)label52).set_Text("d");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(130, 54));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(13, 13));
			((Control)label53).set_TabIndex(176);
			((Control)label53).set_Text("m");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(149, 54));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(13, 13));
			((Control)label54).set_TabIndex(177);
			((Control)label54).set_Text("s");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(112, 54));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(13, 13));
			((Control)label55).set_TabIndex(178);
			((Control)label55).set_Text("h");
			((Control)txtDay).set_Location(new Point(3, 28));
			((TextBoxBase)txtDay).set_MaxLength(2);
			((Control)txtDay).set_Name("txtDay");
			((Control)txtDay).set_Size(new Size(20, 20));
			((Control)txtDay).set_TabIndex(4);
			((Control)txtDay).set_Text("0");
			txtDay.set_TextAlign((HorizontalAlignment)1);
			((Control)txtDay).add_TextChanged((EventHandler)txtDay_TextChanged);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Location(new Point(8, 15));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(13, 13));
			((Control)label44).set_TabIndex(1);
			((Control)label44).set_Text("d");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(3, 2));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(153, 13));
			((Control)label2).set_TabIndex(0);
			((Control)label2).set_Text("Reported local event time");
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Location(new Point(3, 64));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(74, 13));
			((Control)label30).set_TabIndex(155);
			((Control)label30).set_Text("UT equivalent");
			((Control)lblEventUT).set_AutoSize(true);
			((Control)lblEventUT).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblEventUT).set_Location(new Point(83, 64));
			((Control)lblEventUT).set_Name("lblEventUT");
			((Control)lblEventUT).set_Size(new Size(79, 13));
			((Control)lblEventUT).set_TabIndex(154);
			((Control)lblEventUT).set_Text("00  00 00 00");
			((Control)optApparent).set_AutoSize(true);
			((Control)optApparent).set_Location(new Point(107, 36));
			((Control)optApparent).set_Name("optApparent");
			((Control)optApparent).set_Size(new Size(54, 17));
			((Control)optApparent).set_TabIndex(10);
			((Control)optApparent).set_Text("'Aprnt'");
			((ButtonBase)optApparent).set_UseVisualStyleBackColor(true);
			optApparent.add_CheckedChanged((EventHandler)optApparent_CheckedChanged);
			((Control)optMean).set_AutoSize(true);
			optMean.set_Checked(true);
			((Control)optMean).set_Location(new Point(107, 21));
			((Control)optMean).set_Name("optMean");
			((Control)optMean).set_Size(new Size(56, 17));
			((Control)optMean).set_TabIndex(9);
			optMean.set_TabStop(true);
			((Control)optMean).set_Text("'Mean'");
			((ButtonBase)optMean).set_UseVisualStyleBackColor(true);
			optMean.add_CheckedChanged((EventHandler)optMean_CheckedChanged);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Location(new Point(462, 6));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(95, 26));
			((Control)label31).set_TabIndex(18);
			((Control)label31).set_Text("Time difference for\r\naltitude 1' higher");
			((Control)lbldT_RiseSet).set_AutoSize(true);
			((Control)lbldT_RiseSet).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbldT_RiseSet).set_Location(new Point(466, 52));
			((Control)lbldT_RiseSet).set_Name("lbldT_RiseSet");
			((Control)lbldT_RiseSet).set_Size(new Size(70, 14));
			((Control)lbldT_RiseSet).set_TabIndex(19);
			((Control)lbldT_RiseSet).set_Text("± 10 secs");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)resetDateObservationsToolStripMenuItem,
				(ToolStripItem)downloadUpdatesToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(778, 24));
			((Control)menuStrip1).set_TabIndex(156);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)resetDateObservationsToolStripMenuItem).set_Font(new Font("Segoe UI", 9f, FontStyle.Bold));
			((ToolStripItem)resetDateObservationsToolStripMenuItem).set_Name("resetDateObservationsToolStripMenuItem");
			((ToolStripItem)resetDateObservationsToolStripMenuItem).set_Size(new Size(177, 20));
			((ToolStripItem)resetDateObservationsToolStripMenuItem).set_Text("Reset Date && observations   ");
			((ToolStripItem)resetDateObservationsToolStripMenuItem).add_Click((EventHandler)resetDateObservationsToolStripMenuItem_Click);
			((ToolStripDropDownItem)downloadUpdatesToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)starNamesToolStripMenuItem,
				(ToolStripItem)observersToolStripMenuItem
			});
			((ToolStripItem)downloadUpdatesToolStripMenuItem).set_Name("downloadUpdatesToolStripMenuItem");
			((ToolStripItem)downloadUpdatesToolStripMenuItem).set_Size(new Size(139, 20));
			((ToolStripItem)downloadUpdatesToolStripMenuItem).set_Text("Download updates...    ");
			((ToolStripItem)starNamesToolStripMenuItem).set_Name("starNamesToolStripMenuItem");
			((ToolStripItem)starNamesToolStripMenuItem).set_Size(new Size(132, 22));
			((ToolStripItem)starNamesToolStripMenuItem).set_Text("Star names");
			((ToolStripItem)starNamesToolStripMenuItem).add_Click((EventHandler)starNamesToolStripMenuItem_Click);
			((ToolStripItem)observersToolStripMenuItem).set_Name("observersToolStripMenuItem");
			((ToolStripItem)observersToolStripMenuItem).set_Size(new Size(132, 22));
			((ToolStripItem)observersToolStripMenuItem).set_Text("Observers");
			((ToolStripItem)observersToolStripMenuItem).add_Click((EventHandler)observersToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(65, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit    ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(86, 17));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(91, 14));
			((Control)label39).set_TabIndex(4);
			((Control)label39).set_Text(" d  hr  m  s");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(339, 18));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(91, 14));
			((Control)label40).set_TabIndex(14);
			((Control)label40).set_Text(" d  hr  m  s");
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(218, 18));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(91, 14));
			((Control)label41).set_TabIndex(9);
			((Control)label41).set_Text(" d  hr  m  s");
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)cmdTFApparentSet);
			((Control)panel2).get_Controls().Add((Control)(object)cmdTFApparentRise);
			((Control)panel2).get_Controls().Add((Control)(object)cmdTFApparentTransit);
			((Control)panel2).get_Controls().Add((Control)(object)cmdTFMeanSet);
			((Control)panel2).get_Controls().Add((Control)(object)cmdTFMeanRise);
			((Control)panel2).get_Controls().Add((Control)(object)cmdTFMeanTransit);
			((Control)panel2).get_Controls().Add((Control)(object)label41);
			((Control)panel2).get_Controls().Add((Control)(object)label40);
			((Control)panel2).get_Controls().Add((Control)(object)label39);
			((Control)panel2).get_Controls().Add((Control)(object)lbldT_RiseSet);
			((Control)panel2).get_Controls().Add((Control)(object)label31);
			((Control)panel2).get_Controls().Add((Control)(object)label29);
			((Control)panel2).get_Controls().Add((Control)(object)lblApparentRise);
			((Control)panel2).get_Controls().Add((Control)(object)lblApparentSet);
			((Control)panel2).get_Controls().Add((Control)(object)lblApparentTransit);
			((Control)panel2).get_Controls().Add((Control)(object)label23);
			((Control)panel2).get_Controls().Add((Control)(object)lblMeanRise);
			((Control)panel2).get_Controls().Add((Control)(object)lblMeanSet);
			((Control)panel2).get_Controls().Add((Control)(object)lblMeanTransit);
			((Control)panel2).get_Controls().Add((Control)(object)label18);
			((Control)panel2).get_Controls().Add((Control)(object)lblUTRise);
			((Control)panel2).get_Controls().Add((Control)(object)lblUTSet);
			((Control)panel2).get_Controls().Add((Control)(object)label22);
			((Control)panel2).get_Controls().Add((Control)(object)label21);
			((Control)panel2).get_Controls().Add((Control)(object)label19);
			((Control)panel2).get_Controls().Add((Control)(object)lblUTTransit);
			((Control)panel2).set_Location(new Point(17, 270));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(567, 86));
			((Control)panel2).set_TabIndex(10);
			((Control)cmdTFApparentSet).set_Font(new Font("Microsoft Sans Serif", 5.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTFApparentSet).set_Location(new Point(433, 62));
			((Control)cmdTFApparentSet).set_Name("cmdTFApparentSet");
			((Control)cmdTFApparentSet).set_Size(new Size(15, 14));
			((Control)cmdTFApparentSet).set_TabIndex(25);
			((Control)cmdTFApparentSet).set_Text("T");
			((ButtonBase)cmdTFApparentSet).set_UseVisualStyleBackColor(true);
			((Control)cmdTFApparentSet).add_Click((EventHandler)cmdTFApparentSet_Click);
			((Control)cmdTFApparentRise).set_Font(new Font("Microsoft Sans Serif", 5.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTFApparentRise).set_Location(new Point(433, 47));
			((Control)cmdTFApparentRise).set_Name("cmdTFApparentRise");
			((Control)cmdTFApparentRise).set_Size(new Size(15, 14));
			((Control)cmdTFApparentRise).set_TabIndex(24);
			((Control)cmdTFApparentRise).set_Text("T");
			((ButtonBase)cmdTFApparentRise).set_UseVisualStyleBackColor(true);
			((Control)cmdTFApparentRise).add_Click((EventHandler)cmdTFApparentRise_Click);
			((Control)cmdTFApparentTransit).set_Font(new Font("Microsoft Sans Serif", 5.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTFApparentTransit).set_Location(new Point(433, 33));
			((Control)cmdTFApparentTransit).set_Name("cmdTFApparentTransit");
			((Control)cmdTFApparentTransit).set_Size(new Size(15, 14));
			((Control)cmdTFApparentTransit).set_TabIndex(23);
			((Control)cmdTFApparentTransit).set_Text("T");
			((ButtonBase)cmdTFApparentTransit).set_UseVisualStyleBackColor(true);
			((Control)cmdTFApparentTransit).add_Click((EventHandler)cmdTFApparentTransit_Click);
			((Control)cmdTFMeanSet).set_Font(new Font("Microsoft Sans Serif", 5.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTFMeanSet).set_Location(new Point(312, 62));
			((Control)cmdTFMeanSet).set_Name("cmdTFMeanSet");
			((Control)cmdTFMeanSet).set_Size(new Size(15, 14));
			((Control)cmdTFMeanSet).set_TabIndex(22);
			((Control)cmdTFMeanSet).set_Text("T");
			((ButtonBase)cmdTFMeanSet).set_UseVisualStyleBackColor(true);
			((Control)cmdTFMeanSet).add_Click((EventHandler)cmdTFMeanSet_Click);
			((Control)cmdTFMeanRise).set_Font(new Font("Microsoft Sans Serif", 5.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTFMeanRise).set_Location(new Point(312, 47));
			((Control)cmdTFMeanRise).set_Name("cmdTFMeanRise");
			((Control)cmdTFMeanRise).set_Size(new Size(15, 14));
			((Control)cmdTFMeanRise).set_TabIndex(21);
			((Control)cmdTFMeanRise).set_Text("T");
			((ButtonBase)cmdTFMeanRise).set_UseVisualStyleBackColor(true);
			((Control)cmdTFMeanRise).add_Click((EventHandler)cmdTFMeanRise_Click);
			((Control)cmdTFMeanTransit).set_Font(new Font("Microsoft Sans Serif", 5.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTFMeanTransit).set_Location(new Point(312, 33));
			((Control)cmdTFMeanTransit).set_Name("cmdTFMeanTransit");
			((Control)cmdTFMeanTransit).set_Size(new Size(15, 14));
			((Control)cmdTFMeanTransit).set_TabIndex(20);
			((Control)cmdTFMeanTransit).set_Text("T");
			((ButtonBase)cmdTFMeanTransit).set_UseVisualStyleBackColor(true);
			((Control)cmdTFMeanTransit).add_Click((EventHandler)cmdTFMeanTransit_Click);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label11);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label12);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label13);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)txtLatS);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)txtLatM);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)txtLatD);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)txtLongS);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)txtLongM);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)txtLongD);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)txtAltitude);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label24);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label25);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label20);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label10);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label14);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label15);
			((Control)pnlSiteCoords).get_Controls().Add((Control)(object)label16);
			((Control)pnlSiteCoords).set_Location(new Point(297, 4));
			((Control)pnlSiteCoords).set_Name("pnlSiteCoords");
			((Control)pnlSiteCoords).set_Size(new Size(251, 45));
			((Control)pnlSiteCoords).set_TabIndex(2);
			panel3.set_BorderStyle((BorderStyle)1);
			((Control)panel3).get_Controls().Add((Control)(object)pnlSiteCoords);
			((Control)panel3).get_Controls().Add((Control)(object)label1);
			((Control)panel3).get_Controls().Add((Control)(object)cmbObserver);
			((Control)panel3).set_Location(new Point(17, 40));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(566, 60));
			((Control)panel3).set_TabIndex(8);
			panel4.set_BorderStyle((BorderStyle)1);
			((Control)panel4).get_Controls().Add((Control)(object)cmdAddComment);
			((Control)panel4).get_Controls().Add((Control)(object)label61);
			((Control)panel4).get_Controls().Add((Control)(object)cmdSave);
			((Control)panel4).get_Controls().Add((Control)(object)cmdPastOutput);
			((Control)panel4).get_Controls().Add((Control)(object)cmdCopy);
			((Control)panel4).get_Controls().Add((Control)(object)cmdAddClockCorrnToList);
			((Control)panel4).get_Controls().Add((Control)(object)label58);
			((Control)panel4).get_Controls().Add((Control)(object)label57);
			((Control)panel4).get_Controls().Add((Control)(object)label56);
			((Control)panel4).get_Controls().Add((Control)(object)lblMeanCorrectedForRefraction);
			((Control)panel4).get_Controls().Add((Control)(object)lblLinearCorrectedForRefraction);
			((Control)panel4).get_Controls().Add((Control)(object)clbObservations);
			((Control)panel4).get_Controls().Add((Control)(object)lblLinearNoRefractionCorrn);
			((Control)panel4).get_Controls().Add((Control)(object)label47);
			((Control)panel4).get_Controls().Add((Control)(object)lblMean_NoRefractionCorrn);
			((Control)panel4).get_Controls().Add((Control)(object)label45);
			((Control)panel4).get_Controls().Add((Control)(object)cmdDuplicate);
			((Control)panel4).get_Controls().Add((Control)(object)cmdDeleteItemFromList);
			((Control)panel4).get_Controls().Add((Control)(object)label46);
			((Control)panel4).get_Controls().Add((Control)(object)cmdAddRise);
			((Control)panel4).get_Controls().Add((Control)(object)cmdAddSet);
			((Control)panel4).get_Controls().Add((Control)(object)cmdAddTransit);
			((Control)panel4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panel4).set_Location(new Point(17, 369));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(753, 243));
			((Control)panel4).set_TabIndex(11);
			((Control)cmdAddComment).set_BackColor(Color.FromArgb(255, 224, 192));
			((Control)cmdAddComment).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddComment).set_Location(new Point(642, 60));
			((Control)cmdAddComment).set_Name("cmdAddComment");
			((Control)cmdAddComment).set_Size(new Size(102, 35));
			((Control)cmdAddComment).set_TabIndex(8);
			((Control)cmdAddComment).set_Text("add Comment");
			((ButtonBase)cmdAddComment).set_UseVisualStyleBackColor(false);
			((Control)cmdAddComment).add_Click((EventHandler)cmdAddComment_Click);
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(203, 4));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(258, 13));
			((Control)label61).set_TabIndex(12);
			((Control)label61).set_Text("Observations, events, and clock corrections");
			((Control)cmdSave).set_BackColor(Color.FromArgb(255, 255, 192));
			((Control)cmdSave).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdSave).set_Location(new Point(642, 189));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(102, 35));
			((Control)cmdSave).set_TabIndex(11);
			((Control)cmdSave).set_Text("Append list to \r\nLog file\r\n");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(false);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((Control)cmdPastOutput).set_BackColor(Color.FromArgb(255, 192, 255));
			((Control)cmdPastOutput).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdPastOutput).set_Location(new Point(642, 103));
			((Control)cmdPastOutput).set_Name("cmdPastOutput");
			((Control)cmdPastOutput).set_Size(new Size(102, 35));
			((Control)cmdPastOutput).set_TabIndex(9);
			((Control)cmdPastOutput).set_Text("Paste observations");
			((ButtonBase)cmdPastOutput).set_UseVisualStyleBackColor(false);
			((Control)cmdPastOutput).add_Click((EventHandler)cmdPastOutput_Click);
			((Control)cmdCopy).set_BackColor(Color.FromArgb(192, 192, 255));
			((Control)cmdCopy).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdCopy).set_Location(new Point(641, 146));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(102, 35));
			((Control)cmdCopy).set_TabIndex(10);
			((Control)cmdCopy).set_Text("Copy output list");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(false);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			((Control)cmdAddClockCorrnToList).set_BackColor(Color.FromArgb(255, 224, 192));
			((Control)cmdAddClockCorrnToList).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddClockCorrnToList).set_Location(new Point(642, 17));
			((Control)cmdAddClockCorrnToList).set_Name("cmdAddClockCorrnToList");
			((Control)cmdAddClockCorrnToList).set_Size(new Size(102, 35));
			((Control)cmdAddClockCorrnToList).set_TabIndex(7);
			((Control)cmdAddClockCorrnToList).set_Text("add Clock\r\nCorrection");
			((ButtonBase)cmdAddClockCorrnToList).set_UseVisualStyleBackColor(false);
			((Control)cmdAddClockCorrnToList).add_Click((EventHandler)cmdAddClockCorrnToList_Click);
			((Control)label58).set_Anchor((AnchorStyles)6);
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Location(new Point(98, 224));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(160, 13));
			((Control)label58).set_TabIndex(15);
			((Control)label58).set_Text("Refraction removed from altitude");
			((Control)label57).set_Anchor((AnchorStyles)6);
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Location(new Point(98, 210));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(118, 13));
			((Control)label57).set_TabIndex(14);
			((Control)label57).set_Text("No refraction correction");
			((Control)label56).set_Anchor((AnchorStyles)6);
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label56).set_Location(new Point(98, 196));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(100, 13));
			((Control)label56).set_TabIndex(13);
			((Control)label56).set_Text("Clock correction");
			((Control)lblMeanCorrectedForRefraction).set_Anchor((AnchorStyles)6);
			((Control)lblMeanCorrectedForRefraction).set_AutoSize(true);
			((Control)lblMeanCorrectedForRefraction).set_Location(new Point(270, 224));
			((Control)lblMeanCorrectedForRefraction).set_Name("lblMeanCorrectedForRefraction");
			((Control)lblMeanCorrectedForRefraction).set_Size(new Size(38, 13));
			((Control)lblMeanCorrectedForRefraction).set_TabIndex(18);
			((Control)lblMeanCorrectedForRefraction).set_Text("0 secs");
			((Control)lblLinearCorrectedForRefraction).set_Anchor((AnchorStyles)6);
			((Control)lblLinearCorrectedForRefraction).set_AutoSize(true);
			((Control)lblLinearCorrectedForRefraction).set_Location(new Point(372, 224));
			((Control)lblLinearCorrectedForRefraction).set_Name("lblLinearCorrectedForRefraction");
			((Control)lblLinearCorrectedForRefraction).set_Size(new Size(101, 13));
			((Control)lblLinearCorrectedForRefraction).set_TabIndex(21);
			((Control)lblLinearCorrectedForRefraction).set_Text("0 + 0 * (H - 12) secs");
			((Control)clbObservations).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)clbObservations).set_FormattingEnabled(true);
			((ListBox)clbObservations).set_HorizontalScrollbar(true);
			((Control)clbObservations).set_Location(new Point(94, 20));
			((Control)clbObservations).set_Name("clbObservations");
			((ListBox)clbObservations).set_ScrollAlwaysVisible(true);
			((Control)clbObservations).set_Size(new Size(541, 169));
			((ListBox)clbObservations).set_Sorted(true);
			((Control)clbObservations).set_TabIndex(0);
			((Control)clbObservations).add_MouseUp(new MouseEventHandler(clbObservations_MouseUp));
			((Control)lblLinearNoRefractionCorrn).set_Anchor((AnchorStyles)6);
			((Control)lblLinearNoRefractionCorrn).set_AutoSize(true);
			((Control)lblLinearNoRefractionCorrn).set_Location(new Point(372, 210));
			((Control)lblLinearNoRefractionCorrn).set_Name("lblLinearNoRefractionCorrn");
			((Control)lblLinearNoRefractionCorrn).set_Size(new Size(101, 13));
			((Control)lblLinearNoRefractionCorrn).set_TabIndex(20);
			((Control)lblLinearNoRefractionCorrn).set_Text("0 + 0 * (H - 12) secs");
			((Control)label47).set_Anchor((AnchorStyles)6);
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(372, 196));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(57, 13));
			((Control)label47).set_TabIndex(19);
			((Control)label47).set_Text("Linear fit");
			((Control)lblMean_NoRefractionCorrn).set_Anchor((AnchorStyles)6);
			((Control)lblMean_NoRefractionCorrn).set_AutoSize(true);
			((Control)lblMean_NoRefractionCorrn).set_Location(new Point(270, 210));
			((Control)lblMean_NoRefractionCorrn).set_Name("lblMean_NoRefractionCorrn");
			((Control)lblMean_NoRefractionCorrn).set_Size(new Size(38, 13));
			((Control)lblMean_NoRefractionCorrn).set_TabIndex(17);
			((Control)lblMean_NoRefractionCorrn).set_Text("0 secs");
			((Control)label45).set_Anchor((AnchorStyles)6);
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(270, 196));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(38, 13));
			((Control)label45).set_TabIndex(16);
			((Control)label45).set_Text("Mean");
			((Control)cmdDuplicate).set_BackColor(Color.FromArgb(192, 255, 255));
			((Control)cmdDuplicate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDuplicate).set_Location(new Point(5, 131));
			((Control)cmdDuplicate).set_Name("cmdDuplicate");
			((Control)cmdDuplicate).set_Size(new Size(80, 29));
			((Control)cmdDuplicate).set_TabIndex(5);
			((Control)cmdDuplicate).set_Text("Duplicate");
			((ButtonBase)cmdDuplicate).set_UseVisualStyleBackColor(false);
			((Control)cmdDuplicate).add_Click((EventHandler)cmdDuplicate_Click);
			((Control)cmdDeleteItemFromList).set_BackColor(Color.FromArgb(255, 192, 192));
			((Control)cmdDeleteItemFromList).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDeleteItemFromList).set_Location(new Point(5, 168));
			((Control)cmdDeleteItemFromList).set_Name("cmdDeleteItemFromList");
			((Control)cmdDeleteItemFromList).set_Size(new Size(80, 29));
			((Control)cmdDeleteItemFromList).set_TabIndex(6);
			((Control)cmdDeleteItemFromList).set_Text("Delete");
			((ButtonBase)cmdDeleteItemFromList).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteItemFromList).add_Click((EventHandler)cmdDeleteItemFromList_Click);
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Location(new Point(8, 4));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(70, 13));
			((Control)label46).set_TabIndex(1);
			((Control)label46).set_Text("with events...");
			((Control)cmdAddRise).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)cmdAddRise).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddRise).set_Location(new Point(5, 57));
			((Control)cmdAddRise).set_Name("cmdAddRise");
			((Control)cmdAddRise).set_Size(new Size(80, 29));
			((Control)cmdAddRise).set_TabIndex(3);
			((Control)cmdAddRise).set_Text("add Rise");
			((ButtonBase)cmdAddRise).set_UseVisualStyleBackColor(false);
			((Control)cmdAddRise).add_Click((EventHandler)cmdAddRise_Click);
			((Control)cmdAddSet).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)cmdAddSet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddSet).set_Location(new Point(5, 94));
			((Control)cmdAddSet).set_Name("cmdAddSet");
			((Control)cmdAddSet).set_Size(new Size(80, 29));
			((Control)cmdAddSet).set_TabIndex(4);
			((Control)cmdAddSet).set_Text("add Set");
			((ButtonBase)cmdAddSet).set_UseVisualStyleBackColor(false);
			((Control)cmdAddSet).add_Click((EventHandler)cmdAddSet_Click);
			((Control)cmdAddTransit).set_BackColor(Color.FromArgb(192, 255, 192));
			((Control)cmdAddTransit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAddTransit).set_Location(new Point(5, 20));
			((Control)cmdAddTransit).set_Name("cmdAddTransit");
			((Control)cmdAddTransit).set_Size(new Size(80, 29));
			((Control)cmdAddTransit).set_TabIndex(2);
			((Control)cmdAddTransit).set_Text("add Transit");
			((ButtonBase)cmdAddTransit).set_UseVisualStyleBackColor(false);
			((Control)cmdAddTransit).add_Click((EventHandler)cmdAddTransit_Click);
			panel5.set_BorderStyle((BorderStyle)1);
			((Control)panel5).get_Controls().Add((Control)(object)label30);
			((Control)panel5).get_Controls().Add((Control)(object)label52);
			((Control)panel5).get_Controls().Add((Control)(object)label28);
			((Control)panel5).get_Controls().Add((Control)(object)label53);
			((Control)panel5).get_Controls().Add((Control)(object)label27);
			((Control)panel5).get_Controls().Add((Control)(object)label54);
			((Control)panel5).get_Controls().Add((Control)(object)label26);
			((Control)panel5).get_Controls().Add((Control)(object)label55);
			((Control)panel5).get_Controls().Add((Control)(object)txtH);
			((Control)panel5).get_Controls().Add((Control)(object)txtM);
			((Control)panel5).get_Controls().Add((Control)(object)txtS);
			((Control)panel5).get_Controls().Add((Control)(object)optMean);
			((Control)panel5).get_Controls().Add((Control)(object)optApparent);
			((Control)panel5).get_Controls().Add((Control)(object)lblEventUT);
			((Control)panel5).get_Controls().Add((Control)(object)txtDay);
			((Control)panel5).get_Controls().Add((Control)(object)label2);
			((Control)panel5).get_Controls().Add((Control)(object)label44);
			((Control)panel5).set_Location(new Point(604, 40));
			((Control)panel5).set_Name("panel5");
			((Control)panel5).set_Size(new Size(166, 84));
			((Control)panel5).set_TabIndex(162);
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Symbol", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 2));
			((Control)label63).set_Location(new Point(6, 202));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(28, 13));
			((Control)label63).set_TabIndex(181);
			((Control)label63).set_Text("DT=");
			((Control)lbldeltaT).set_AutoSize(true);
			((Control)lbldeltaT).set_Location(new Point(33, 202));
			((Control)lbldeltaT).set_Name("lbldeltaT");
			((Control)lbldeltaT).set_Size(new Size(13, 13));
			((Control)lbldeltaT).set_TabIndex(182);
			((Control)lbldeltaT).set_Text("0");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(778, 619));
			((Control)this).get_Controls().Add((Control)(object)panel5);
			((Control)this).get_Controls().Add((Control)(object)panel4);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)panel2);
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)panelAlt);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_Name("EventTimeFromStarAltitude");
			((Control)this).set_Text("Event time from the altitude of a star");
			((Form)this).add_Load((EventHandler)EventTimeFromStarAltitude_Load);
			((Control)this).add_Resize((EventHandler)EventTimeFromStarAltitude_Resize);
			((ISupportInitialize)updnYear).EndInit();
			((Control)panelAlt).ResumeLayout(false);
			((Control)panelAlt).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panel6).ResumeLayout(false);
			((Control)panel6).PerformLayout();
			((Control)panel7).ResumeLayout(false);
			((Control)panel7).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)pnlSiteCoords).ResumeLayout(false);
			((Control)pnlSiteCoords).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((Control)panel5).ResumeLayout(false);
			((Control)panel5).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
