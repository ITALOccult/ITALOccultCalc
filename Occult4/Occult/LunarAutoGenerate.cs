using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.IO;
using System.IO.Compression;
using System.Text;
using System.Windows.Forms;
using Occult.Lunar_Occultations;
using Occult.Properties;

namespace Occult
{
	public class LunarAutoGenerate : Form
	{
		private readonly string AppPath;

		private readonly string StarCat0;

		private readonly string StarCat1;

		private readonly string StarCat2;

		private readonly string StarCat3;

		private readonly string StarCat4;

		private const double Radian = 180.0 / Math.PI;

		private bool FormCreated;

		private string StarCatalogue;

		private string SiteFileName;

		private static bool CancelFlag;

		private static bool DatesSet;

		private int SiteCount;

		private Sites S;

		private Sites CurrentSite = new Sites();

		private List<Sites> AllSites = new List<Sites>();

		internal ArrayList lstPrediction_Index = new ArrayList();

		private double JDFirstDate;

		private double EastLimit;

		private double WestLimit;

		private double NorthLimit;

		private double SouthLimit;

		private double MaxAperture;

		public static readonly int[] DaysInMonth = new int[12]
		{
			31, 28, 31, 30, 31, 30, 31, 31, 30, 31,
			30, 31
		};

		private IContainer components;

		private CheckBox chkFilter;

		private GroupBox grpObjects;

		private CheckBox chkAsteroids;

		private CheckBox chkStars;

		private CheckBox chkPlanets;

		private GroupBox grpXZ80;

		private RadioButton optXZ9;

		private RadioButton optXZ6;

		private RadioButton optXZ3;

		private RadioButton optZC;

		private RadioButton optXZ;

		private ComboBox cmbSiteFiles;

		private Label label28;

		private ComboBox cmbLunarDayEnd;

		private ComboBox cmbLunarDayStart;

		private ComboBox cmbLunarMonthEnd;

		private ComboBox cmbLunarMonthStart;

		private NumericUpDown updnLunarYearEnd;

		private NumericUpDown updnLunarYearStart;

		internal Label labelIntegration;

		private Button cmdOccultationsForAll;

		private Button cmdGrazes;

		private Button cmdCancel;

		private ProgressBar pBar2;

		internal ProgressBar pBar;

		private Label lblCurrentDate;

		private CheckBox chkZip;

		private CheckBox chkDelete;

		private CheckedListBox lstSites;

		private Button cmdAll;

		private Button cmdNone;

		private Label label1;

		private GroupBox grpOutputs;

		private Label label2;

		private RadioButton optLarge;

		private RadioButton optMedium;

		private RadioButton optSmall;

		private ComboBox cmbFileType;

		private Label label3;

		private GroupBox groupBox1;

		private GroupBox groupBox2;

		private Label label4;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label lblCount;

		private Label label50;

		private ComboBox cmbLibrations;

		private CheckBox chkIncludeKML;

		private Label label8;

		private CheckBox chkIncludeProfile;

		private CheckBox chkIncludeHTML;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Button button1;

		private Button cmdFullMonth;

		private Button cmdOneDay;

		private Button cmdFullYear;

		private GroupBox groupBox3;

		private GroupBox groupBox4;

		private CheckBox chkIncludeText;

		private CheckBox chkIncludeLocalHTML;

		private Label label13;

		private CheckBox chkRegional;

		private Label label14;

		private Label label15;

		private Label label16;

		private Label label17;

		private Label lblRange;

		private Label label18;

		private Button cmd3Mths;

		private Label label20;

		private Label label19;

		private CheckBox chkSpaces;

		private GroupBox groupBox5;

		private Button cmdComputeFixed;

		private Label label21;

		private NumericUpDown updnFixedYear;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ComboBox cmbMonths;

		private Label label22;

		private CheckBox chkUsePoints;

		private CheckBox chkUseHires;

		private CheckBox chkUseLoRes;

		private CheckBox chkShortOutput;

		private ComboBox cmbLimitingMagnitude;

		private Label label23;

		private GroupBox groupBox6;

		private Button cmdMergeBAA;

		private CheckBox chkNumberOfEvents;

		private CheckBox chkUseCA;

		private Panel panel1;

		private RadioButton optLOLA;

		private Panel panel2;

		private Label label24;

		private RadioButton optBandW;

		private RadioButton optColor;

		public LunarAutoGenerate()
		{
			InitializeComponent();
			AppPath = Utilities.AppPath;
			StarCat0 = AppPath + "\\Resource Files\\XZ80.dat";
			StarCat1 = AppPath + "\\Resource Files\\ZC.dat";
			StarCat2 = AppPath + "\\Resource Files\\XZ80Mag4.dat";
			StarCat3 = AppPath + "\\Resource Files\\XZ80Mag7.dat";
			StarCatalogue = (StarCat4 = AppPath + "\\Resource Files\\XZ80Mag9.dat");
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
		}

		private void LunarAutoGenerate_Load(object sender, EventArgs e)
		{
			//IL_02a3: Unknown result type (might be due to invalid IL or missing references)
			//IL_048a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0496: Unknown result type (might be due to invalid IL or missing references)
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			FormCreated = false;
			DateTime now = DateTime.Now;
			DatesSet = false;
			NumericUpDown obj = updnLunarYearStart;
			NumericUpDown obj2 = updnLunarYearEnd;
			decimal num;
			updnFixedYear.set_Value(num = now.ToUniversalTime().Year);
			decimal value;
			obj2.set_Value(value = num);
			obj.set_Value(value);
			ComboBox obj3 = cmbLunarMonthStart;
			int selectedIndex;
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(selectedIndex = now.ToUniversalTime().Month - 1);
			((ListControl)obj3).set_SelectedIndex(selectedIndex);
			DatesSet = true;
			ComboBox obj4 = cmbLunarDayStart;
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(selectedIndex = now.ToUniversalTime().Day - 1);
			((ListControl)obj4).set_SelectedIndex(selectedIndex);
			((ListControl)cmbFileType).set_SelectedIndex(Settings.Default.GrazeProfile_FileFormat);
			((ListControl)cmbLibrations).set_SelectedIndex(Settings.Default.GrazeProfile_LibrationPlot);
			optSmall.set_Checked(Settings.Default.GrazeProfileSaveSmall);
			optMedium.set_Checked(Settings.Default.GrazeProfileSaveMedium);
			optLarge.set_Checked(Settings.Default.GrazeProfileSaveLarge);
			optColor.set_Checked(Settings.Default.GrazeProfile_EventsInColour);
			optBandW.set_Checked(Settings.Default.BWFlag);
			((ListControl)cmbMonths).set_SelectedIndex(0);
			CheckBox obj5 = chkIncludeProfile;
			CheckBox obj6 = chkUseHires;
			CheckBox obj7 = chkUseLoRes;
			bool lOLAFileExists;
			((Control)chkUsePoints).set_Enabled(lOLAFileExists = Utilities.LOLAFileExists);
			bool flag;
			((Control)obj7).set_Enabled(flag = lOLAFileExists);
			bool enabled;
			((Control)obj6).set_Enabled(enabled = flag);
			((Control)obj5).set_Enabled(enabled);
			((ListControl)cmbLimitingMagnitude).set_SelectedIndex(Settings.Default.LunarMultipleMagLimit);
			((Control)optLOLA).set_Enabled(Utilities.LOLAFileExists);
			if (Utilities.LOLAFileExists)
			{
				optLOLA.set_Checked(true);
			}
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
			}
			if (cmbSiteFiles.get_Items().get_Count() < 0)
			{
				MessageBox.Show("No site files available", "No site files", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			string siteFileLunarMultiPredict = Settings.Default.SiteFileLunarMultiPredict;
			for (int i = 0; i < cmbSiteFiles.get_Items().get_Count(); i++)
			{
				if (siteFileLunarMultiPredict == cmbSiteFiles.get_Items().get_Item(i).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(i);
					break;
				}
			}
			SetStarCats();
			if (!File.Exists(AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv"))
			{
				List<AsteroidElements> astElements = Elements.MainAsteroids.AstElements;
				if (astElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
				using StreamWriter streamWriter = new StreamWriter(AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv");
				streamWriter.WriteLine(AsteroidElements.CSVHeader());
				for (int j = 0; j <= 100; j++)
				{
					double a = astElements[j].A;
					if (astElements[j].H0 + 5.0 * Math.Log10(a * (a - 1.0)) <= 9.0)
					{
						streamWriter.WriteLine(Elements.MainAsteroids.AstElements[j].CSVString());
					}
				}
			}
			if (Elements.UserAsteroids_Lunar.AstElements.Count < 1)
			{
				Elements.UserAsteroids_Lunar.Fill_AllAsteroids();
			}
			if (Elements.UserAsteroids_Lunar.AstElements.Count > 0)
			{
				double osculatingJD = Elements.UserAsteroids_Lunar.AstElements[0].OsculatingJD;
				double num2 = Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day);
				if (Math.Abs(osculatingJD - num2) > 1096.0)
				{
					MessageBox.Show("Epoch of asteroid elements is more than 3 years from the present date.\r\nThe elements should be replaced with a more recent orbit\r\n[using   'Edit the User file of minor planets']\r\n\r\n\r\nPredictions of lunar occultations of asteroids will not be made\r\nif the epoch of the elements differ from the prediction date by\r\nmore than 5 years.\r\n\r\n\r\nYou will now be taken to the editor for the User file of elements. Assuming you have recently updated the file of asteroid elements using ASTORB or MPCORB, the simplest option is to delete all asteroids from the list, click the 'Add' button, then save the new list.", "Check for current asteroid elements.", (MessageBoxButtons)0, (MessageBoxIcon)48);
					((Form)new UserAsteroids_Edit(LunarOccultations: true)).ShowDialog();
				}
			}
			FormCreated = true;
		}

		internal void SetStarCats()
		{
			((Control)optXZ).set_Enabled(File.Exists(StarCat0));
			((Control)optZC).set_Enabled(File.Exists(StarCat1));
			((Control)optXZ3).set_Enabled(File.Exists(StarCat2));
			((Control)optXZ6).set_Enabled(File.Exists(StarCat3));
			((Control)optXZ9).set_Enabled(File.Exists(StarCat4));
		}

		private void cmbSiteFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			EastLimit = -180.0;
			WestLimit = 180.0;
			NorthLimit = -90.0;
			SouthLimit = 90.0;
			MaxAperture = 0.0;
			AllSites.Clear();
			SiteFileName = Path.GetFileNameWithoutExtension(cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
			StreamReader streamReader = new StreamReader(AppPath + "\\sites\\" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()));
			do
			{
				S = new Sites();
				S.Read_SiteFile(streamReader.ReadLine());
				AllSites.Add(S);
				if (S.Longitude > EastLimit)
				{
					EastLimit = S.Longitude;
				}
				if (S.Longitude < WestLimit)
				{
					WestLimit = S.Longitude;
				}
				if (S.Latitude > NorthLimit)
				{
					NorthLimit = S.Latitude;
				}
				if (S.Latitude < SouthLimit)
				{
					SouthLimit = S.Latitude;
				}
				if ((double)S.ApertureCM > MaxAperture)
				{
					MaxAperture = S.ApertureCM;
				}
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			EastLimit += 2.0;
			WestLimit -= 2.0;
			NorthLimit += 2.0;
			SouthLimit -= 2.0;
			((Control)lblRange).set_Text($"{WestLimit:F1} to {EastLimit:F1}, {SouthLimit:F1} to {NorthLimit:F1}");
			Sites.SortField = 0;
			AllSites.Sort();
			((ObjectCollection)lstSites.get_Items()).Clear();
			for (int i = 0; i < AllSites.Count; i++)
			{
				((ObjectCollection)lstSites.get_Items()).Add((object)AllSites[i].Name);
				if (AllSites[i].Name == Settings.Default.ObserverLunarPredict)
				{
					lstSites.SetItemChecked(i, true);
				}
			}
			CountSites();
			if (FormCreated)
			{
				Settings.Default.SiteFileLunarMultiPredict = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
			}
		}

		private void lstSites_MouseUp(object sender, MouseEventArgs e)
		{
			CountSites();
		}

		private void CountSites()
		{
			SiteCount = 0;
			for (int i = 0; i < ((ObjectCollection)lstSites.get_Items()).get_Count(); i++)
			{
				if (lstSites.GetItemChecked(i))
				{
					SiteCount++;
				}
			}
			((Control)lblCount).set_Text(SiteCount + " sites selected");
		}

		private void chkZip_CheckedChanged(object sender, EventArgs e)
		{
			((Control)chkDelete).set_Enabled(chkZip.get_Checked());
		}

		private void cmdNone_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)lstSites.get_Items()).get_Count(); i++)
			{
				lstSites.SetItemChecked(i, false);
			}
			CountSites();
		}

		private void cmdAll_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)lstSites.get_Items()).get_Count(); i++)
			{
				lstSites.SetItemChecked(i, true);
			}
			CountSites();
		}

		private void optXZ_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat0;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
		}

		private void optZC_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat1;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
		}

		private void optXZ3_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat2;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
		}

		private void optXZ6_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat3;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
		}

		private void optXZ9_CheckedChanged(object sender, EventArgs e)
		{
			StarCatalogue = StarCat4;
			LunarOccultations.InitialiseStarCatIndexArray(StarCatalogue);
		}

		private void updnLunarYearStart_ValueChanged(object sender, EventArgs e)
		{
			SetStartDays();
			JDFirstDate = SetDefaultEndDate();
		}

		private void cmbLunarMonthStart_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetStartDays();
			JDFirstDate = SetDefaultEndDate();
		}

		private void SetStartDays()
		{
			if (!DatesSet)
			{
				return;
			}
			int num = DaysInMonth[((ListControl)cmbLunarMonthStart).get_SelectedIndex()];
			int selectedIndex = ((ListControl)cmbLunarDayStart).get_SelectedIndex();
			if (((ListControl)cmbLunarMonthStart).get_SelectedIndex() == 1)
			{
				if ((updnLunarYearStart.get_Value() > 1600m) & (updnLunarYearStart.get_Value() % 400m == 0m))
				{
					num = 29;
				}
				else if (updnLunarYearStart.get_Value() % 100m == 0m)
				{
					num = 28;
				}
				else if (updnLunarYearStart.get_Value() % 4m == 0m)
				{
					num = 29;
				}
			}
			cmbLunarDayStart.get_Items().Clear();
			for (int i = 1; i <= num; i++)
			{
				cmbLunarDayStart.get_Items().Add((object)i);
			}
			if (selectedIndex < cmbLunarDayStart.get_Items().get_Count())
			{
				((ListControl)cmbLunarDayStart).set_SelectedIndex(selectedIndex);
			}
			else
			{
				((ListControl)cmbLunarDayStart).set_SelectedIndex(cmbLunarDayStart.get_Items().get_Count() - 1);
			}
		}

		private void cmbLunarDayStart_SelectedIndexChanged(object sender, EventArgs e)
		{
			JDFirstDate = SetDefaultEndDate();
		}

		private void updnLunarYearEnd_ValueChanged(object sender, EventArgs e)
		{
			SetEndDays();
		}

		private void cmbLunarMonthEnd_SelectedIndexChanged(object sender, EventArgs e)
		{
			SetEndDays();
		}

		private void SetEndDays()
		{
			if (!DatesSet)
			{
				return;
			}
			int num = DaysInMonth[((ListControl)cmbLunarMonthEnd).get_SelectedIndex()];
			int selectedIndex = ((ListControl)cmbLunarDayEnd).get_SelectedIndex();
			if (((ListControl)cmbLunarMonthEnd).get_SelectedIndex() == 1)
			{
				if ((updnLunarYearStart.get_Value() > 1600m) & (updnLunarYearEnd.get_Value() % 400m == 0m))
				{
					num = 29;
				}
				else if (updnLunarYearEnd.get_Value() % 100m == 0m)
				{
					num = 28;
				}
				else if (updnLunarYearEnd.get_Value() % 4m == 0m)
				{
					num = 29;
				}
			}
			cmbLunarDayEnd.get_Items().Clear();
			for (int i = 1; i <= num; i++)
			{
				cmbLunarDayEnd.get_Items().Add((object)i);
			}
			if (selectedIndex < cmbLunarDayEnd.get_Items().get_Count())
			{
				((ListControl)cmbLunarDayEnd).set_SelectedIndex(selectedIndex);
			}
			else
			{
				((ListControl)cmbLunarDayEnd).set_SelectedIndex(cmbLunarDayEnd.get_Items().get_Count() - 1);
			}
		}

		private void cmbLunarDayEnd_SelectedIndexChanged(object sender, EventArgs e)
		{
		}

		private double SetDefaultEndDate()
		{
			int Year = (int)updnLunarYearStart.get_Value();
			int Month = ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1;
			double day = (double)((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1.0;
			double num = Utilities.JD_from_Date(Year, Month, day);
			Utilities.Date_from_JD(num, out Year, out Month, out day);
			updnLunarYearEnd.set_Value((decimal)Year);
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(Month - 1);
			((ListControl)cmbLunarDayEnd).set_SelectedIndex((int)(day - 1.0));
			return num;
		}

		private double GetEndDate()
		{
			int year = (int)updnLunarYearEnd.get_Value();
			int month = ((ListControl)cmbLunarMonthEnd).get_SelectedIndex() + 1;
			double day = (double)((ListControl)cmbLunarDayEnd).get_SelectedIndex() + 1.0;
			return Utilities.JD_from_Date(year, month, day);
		}

		private void cmdOccultationsForAll_Click(object sender, EventArgs e)
		{
			//IL_0016: Unknown result type (might be due to invalid IL or missing references)
			if (SiteCount < 1)
			{
				MessageBox.Show("No sites are selected", "No sites", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				LunarCalculate();
			}
		}

		private void LunarCalculate()
		{
			int[] array = new int[((ObjectCollection)lstSites.get_Items()).get_Count()];
			int[] array2 = new int[((ObjectCollection)lstSites.get_Items()).get_Count()];
			double num = double.Parse(cmbLimitingMagnitude.get_Items().get_Item(((ListControl)cmbLimitingMagnitude).get_SelectedIndex()).ToString());
			bool @checked = chkUseCA.get_Checked();
			string text = $"{updnLunarYearStart.get_Value():F0}" + $"{((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1:F0}".PadLeft(2, '0') + $"{((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1:F0}".PadLeft(2, '0') + " to " + $"{updnLunarYearEnd.get_Value():F0}" + $"{((ListControl)cmbLunarMonthEnd).get_SelectedIndex() + 1:F0}".PadLeft(2, '0') + $"{((ListControl)cmbLunarDayEnd).get_SelectedIndex() + 1:F0}".PadLeft(2, '0') + " ";
			double num2 = Utilities.JD_from_Date((int)updnLunarYearStart.get_Value(), ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1);
			double num3 = Utilities.JD_from_Date((int)updnLunarYearEnd.get_Value(), ((ListControl)cmbLunarMonthEnd).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayEnd).get_SelectedIndex() + 1);
			LunarOccultations.InitialiseSearch();
			LunarOccultations.ListGrazesOnly = false;
			CancelFlag = false;
			((Control)cmdCancel).set_Visible(true);
			for (double num4 = num2; num4 <= num3; num4 += 1.0)
			{
				((Control)lblCurrentDate).set_Text("[" + Utilities.Date_from_JD(num4, 0) + "]");
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
				LunarOccultations.NewDate(num4, 0, chkFilter.get_Checked(), NorthLimit, SouthLimit, EastLimit, WestLimit);
				double readMagLimit = LunarOccultations.GetReadMagLimit(MaxAperture, CurrentSite.MagCorrection);
				LunarOccultations.ScanXZFile(StarCatalogue, NorthLimit, SouthLimit, readMagLimit, DoublesOnly: false, Auto: true);
				for (int i = 0; i < ((ObjectCollection)lstSites.get_Items()).get_Count(); i++)
				{
					if (!lstSites.GetItemChecked(i))
					{
						continue;
					}
					CurrentSite = AllSites[i];
					double longitude = CurrentSite.Longitude / (180.0 / Math.PI);
					string text2 = CurrentSite.Name.Replace(",", "").Replace("+", "_");
					string text3 = AppPath + "\\AutoGenerated Lunar\\" + text2;
					if (!Directory.Exists(text3))
					{
						Directory.CreateDirectory(text3);
					}
					double pCos = CurrentSite.pCos;
					double pSin = CurrentSite.pSin;
					string text4 = text3 + "\\" + text + text2;
					using (StreamWriter streamWriter = new StreamWriter(text4 + ".txt", num4 != num2))
					{
						if (num4 == num2)
						{
							array[i] = 0;
							array2[i] = 1;
							LunarHeader(streamWriter, @checked);
						}
						pBar2.set_Minimum(0);
						pBar2.set_Maximum(LunarOccultations.Elements.Count);
						((Control)pBar2).set_Visible(true);
						LunarOccultations.Prediction.Clear();
						for (int j = 0; j < LunarOccultations.Elements.Count; j++)
						{
							pBar2.set_Value(j);
							LunarOccultations.ComputeEventForALocation(j, longitude, CurrentSite.Latitude, CurrentSite.Altitude, pSin, pCos, CurrentSite.ApertureCM * 10f, CurrentSite.MagCorrection, CurrentSite.GrazeTravelDist, MultiSite: false, 0);
						}
						((Control)pBar2).set_Visible(false);
						LunarOccultations.Prediction.Sort();
						for (int j = 0; j < LunarOccultations.Prediction.Count; j++)
						{
							if (chkShortOutput.get_Checked())
							{
								if (!LunarOccultations.Prediction[j].EventPhase.ToUpper().Contains("G") & !LunarOccultations.Prediction[j].EventPhase.ToUpper().Contains("M") & (LunarOccultations.Prediction[j].Mv <= num))
								{
									streamWriter.WriteLine(LunarOccultations.Prediction[j].BAAPrediction(@checked));
								}
								continue;
							}
							streamWriter.WriteLine(LunarOccultations.Prediction[j].ToString());
							array[i]++;
							if (LunarOccultations.Prediction[j].ZCNameExists)
							{
								streamWriter.WriteLine(LunarOccultations.Prediction[j].ZCName);
								array[i]++;
							}
							if (LunarOccultations.Prediction[j].DoubleDetailsExist)
							{
								streamWriter.WriteLine(LunarOccultations.Prediction[j].DoubleDetails);
								array[i]++;
							}
							if (LunarOccultations.Prediction[j].DoubleWantedExists)
							{
								streamWriter.WriteLine(LunarOccultations.Prediction[j].DoubleObservationsWanted);
								array[i]++;
							}
							if (LunarOccultations.Prediction[j].VariableDetailsExist)
							{
								streamWriter.WriteLine(LunarOccultations.Prediction[j].VariableDetails);
								array[i]++;
							}
							if ((LunarOccultations.Prediction[j].LimbDistance < 20.0) & (LunarOccultations.Prediction[j].LimbDistance > 0.0))
							{
								StringBuilder stringBuilder = new StringBuilder();
								stringBuilder.Append("   Distance of " + LunarOccultations.Prediction[j].StarId.PadRight(7).Substring(0, 7).Trim());
								stringBuilder.AppendFormat(" to Terminator = {0,1:F1}", LunarOccultations.Prediction[j].LimbDistance);
								stringBuilder.AppendFormat("\"; to 3km sunlit peak = {0,1:F1}\"", LunarOccultations.Prediction[j].MountainDistance);
								streamWriter.WriteLine(stringBuilder.ToString());
								array[i]++;
							}
							if (LunarOccultations.Prediction[j].Durn > 0.0)
							{
								streamWriter.WriteLine("   Duration of planetary disk occultation: predicted time +/-" + string.Format("{0,1:F1}", LunarOccultations.Prediction[j].Durn / 2.0) + " secs");
								array[i]++;
							}
							if (array[i] >= 35)
							{
								streamWriter.WriteLine("");
								streamWriter.WriteLine("".PadRight(60) + string.Format("- page {0,1} -", array2[i]));
								streamWriter.WriteLine("");
								streamWriter.WriteLine("");
								streamWriter.WriteLine("");
								LunarHeader(streamWriter, @checked);
								array[i] = 0;
								array2[i]++;
							}
						}
						if (num4 == num3)
						{
							streamWriter.WriteLine("");
							streamWriter.WriteLine("".PadRight(60) + string.Format("- page {0,1} -", array2[i]));
						}
					}
					if (num4 == num3 && File.Exists(text4 + ".txt") && chkZip.get_Checked())
					{
						string text5 = text4 + ".zip";
						if (File.Exists(text5))
						{
							File.Delete(text5);
						}
						using (ZipArchive destination = ZipFile.Open(text5, ZipArchiveMode.Create))
						{
							destination.CreateEntryFromFile(text4 + ".txt", new FileInfo(text4 + ".txt").Name);
						}
						if (chkDelete.get_Checked())
						{
							File.Delete(text4 + ".txt");
						}
					}
				}
			}
			((Control)cmdCancel).set_Visible(false);
		}

		private void LunarHeader(StreamWriter Lunar, bool UseCA)
		{
			if (chkShortOutput.get_Checked())
			{
				Lunar.WriteLine("");
				Lunar.WriteLine("Occultation prediction for " + CurrentSite.Name);
				Lunar.WriteLine($"{CurrentSite.Longitude:E 0.0;W 0.0},  " + $"{CurrentSite.Latitude:N 0.0;S 0.0}");
				Lunar.WriteLine("");
				if (UseCA)
				{
					Lunar.WriteLine("   Date     Star  Mag Ph  Ill   U.T.    a    b   CA");
					Lunar.WriteLine(" y   m  d    No            %   h   m    m    m    o");
				}
				else
				{
					Lunar.WriteLine("   Date     Star  Mag Ph  El    U.T.    a    b    P");
					Lunar.WriteLine(" y   m  d    No            o   h   m    m    m    o");
				}
			}
			else
			{
				Lunar.WriteLine("");
				Lunar.WriteLine("Occultation prediction for " + CurrentSite.Name);
				Lunar.WriteLine("E. Longitude " + Utilities.DEGtoDMS(CurrentSite.Longitude, 4, 1, MinutesOnly: false) + ",  Latitude " + Utilities.DEGtoDMS(CurrentSite.Latitude, 3, 1, MinutesOnly: false) + ",  Alt." + string.Format("{0,5:F0}m", CurrentSite.Altitude) + ";  Telescope dia" + string.Format("{0,3:F0}cm", CurrentSite.ApertureCM) + ";  dMag" + string.Format("{0,4:F1}", CurrentSite.MagCorrection));
				Lunar.WriteLine("");
				Lunar.WriteLine("       day  Time   P   Star  Sp  Mag  Mag    % Elon Sun  Moon   CA   PA  VA  AA Libration   A   B   RV   Cct durn R.A. (J2000)  Dec");
				Lunar.WriteLine(" y   m  d  h  m  s      No  D     v    r V  ill     Alt Alt Az   o    o   o   o   L    B   m/o m/o \"/sec   o sec  h  m   s    o  m  s");
			}
		}

		private void cmdGrazes_Click(object sender, EventArgs e)
		{
			//IL_0042: Unknown result type (might be due to invalid IL or missing references)
			//IL_00c7: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			string text2 = "";
			string SavedFileExtension = "";
			_ = new int[((ObjectCollection)lstSites.get_Items()).get_Count()];
			int num = 1;
			if (SiteCount < 1)
			{
				MessageBox.Show("No sites are selected", "No sites", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			string path = AppPath + "\\AutoGenerated Grazes\\GoogleMap files";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			EastLimit = -180.0;
			WestLimit = 180.0;
			NorthLimit = -90.0;
			SouthLimit = 90.0;
			MaxAperture = 0.0;
			for (int i = 0; i < ((ObjectCollection)lstSites.get_Items()).get_Count(); i++)
			{
				if ((int)lstSites.GetItemCheckState(i) != 0)
				{
					if (AllSites[i].Longitude > EastLimit)
					{
						EastLimit = AllSites[i].Longitude;
					}
					if (AllSites[i].Longitude < WestLimit)
					{
						WestLimit = AllSites[i].Longitude;
					}
					if (AllSites[i].Latitude > NorthLimit)
					{
						NorthLimit = AllSites[i].Latitude;
					}
					if (AllSites[i].Latitude < SouthLimit)
					{
						SouthLimit = AllSites[i].Latitude;
					}
					if ((double)AllSites[i].ApertureCM > MaxAperture)
					{
						MaxAperture = AllSites[i].ApertureCM;
					}
				}
			}
			EastLimit += 2.0;
			WestLimit -= 2.0;
			NorthLimit += 2.0;
			SouthLimit -= 2.0;
			double num2 = Utilities.JD_from_Date((int)updnLunarYearStart.get_Value(), ((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1);
			double num3 = Utilities.JD_from_Date((int)updnLunarYearEnd.get_Value(), ((ListControl)cmbLunarMonthEnd).get_SelectedIndex() + 1, ((ListControl)cmbLunarDayEnd).get_SelectedIndex() + 1);
			if (chkRegional.get_Checked())
			{
				string path2 = AppPath + "\\AutoGenerated Grazes\\tmp";
				if (!Directory.Exists(path2))
				{
					Directory.CreateDirectory(path2);
				}
			}
			LunarOccultations.InitialiseSearch();
			CancelFlag = false;
			((Control)cmdCancel).set_Visible(true);
			if (LunarProfile.GrazeProfile == null)
			{
				LunarProfile.InitialiseProfile();
			}
			((Form)LunarProfile.GrazeProfile).set_WindowState((FormWindowState)1);
			((Control)this).Focus();
			string text3 = $"{updnLunarYearStart.get_Value():F0}" + $"{((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 1:F0}".PadLeft(2, '0') + $"{((ListControl)cmbLunarDayStart).get_SelectedIndex() + 1:F0}".PadLeft(2, '0') + " to " + $"{updnLunarYearEnd.get_Value():F0}" + $"{((ListControl)cmbLunarMonthEnd).get_SelectedIndex() + 1:F0}".PadLeft(2, '0') + $"{((ListControl)cmbLunarDayEnd).get_SelectedIndex() + 1:F0}".PadLeft(2, '0');
			string text4 = text3 + " Graze Summary for ";
			for (double num4 = num2; num4 <= num3; num4 += 1.0)
			{
				((Control)lblCurrentDate).set_Text("[" + Utilities.Date_from_JD(num4, 0) + "]");
				Utilities.Date_from_JD(num4, out var Year, out var Month, out var day);
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
				LunarOccultations.NewDate(num4, 0, chkFilter.get_Checked(), NorthLimit, SouthLimit, EastLimit, WestLimit);
				double readMagLimit = LunarOccultations.GetReadMagLimit(MaxAperture, CurrentSite.MagCorrection);
				LunarOccultations.ListGrazesOnly = true;
				LunarProfile.GetDataFiles();
				LunarOccultations.ScanXZFile(StarCatalogue, NorthLimit, SouthLimit, readMagLimit, DoublesOnly: false, Auto: true);
				for (int i = 0; i < ((ObjectCollection)lstSites.get_Items()).get_Count(); i++)
				{
					if (!lstSites.GetItemChecked(i))
					{
						continue;
					}
					CurrentSite = AllSites[i];
					double longitude = CurrentSite.Longitude / (180.0 / Math.PI);
					string text5 = CurrentSite.Name.Replace(",", "").Replace("+", "_").Replace("/", "_");
					string path2 = AppPath + "\\AutoGenerated Grazes\\" + text5;
					if (!Directory.Exists(path2))
					{
						Directory.CreateDirectory(path2);
					}
					string path3 = ((!Settings.Default.LunarNoSpacesInFileName) ? (path2 + "\\" + text4 + text5 + " Summary.txt") : (path2 + "\\" + (text4 + text5 + " Summary.txt").Replace(" ", "_")));
					double pCos = CurrentSite.pCos;
					double pSin = CurrentSite.pSin;
					if (num4 == JDFirstDate)
					{
						using StreamWriter streamWriter = new StreamWriter(path3, append: false);
						streamWriter.WriteLine("Graze Summary for " + CurrentSite.Name);
						streamWriter.WriteLine("");
						streamWriter.WriteLine("For the period " + Utilities.Date_from_JD(num2, 0) + " to " + Utilities.Date_from_JD(num3, 0));
						streamWriter.WriteLine("");
						streamWriter.WriteLine("Station Longitude : " + $"{CurrentSite.Longitude:F3}");
						streamWriter.WriteLine("Station Latitude  : " + $"{CurrentSite.Latitude:F3}");
						streamWriter.WriteLine("Graze travel distances. General = {0:F0}km; <Mag 6.5 = {1:F0}km; <Mag 4.5 = {2:F0}km", CurrentSite.GrazeTravelDist, Settings.Default.Graze_TravelDistance_65, Settings.Default.Graze_TravelDistance_45);
						streamWriter.WriteLine("Telescope aperture used for graze selection = {0:F0}cm", CurrentSite.ApertureCM);
						streamWriter.WriteLine("");
						streamWriter.WriteLine("       day  Time       Star  Sp  Mag  Mag    % Elon Sun Moon     Cusp angle  Distance");
						streamWriter.WriteLine(" y   m  d  h  m  s      No  D     v    r V  ill     Alt Alt Az       o          km ");
					}
					pBar2.set_Minimum(0);
					pBar2.set_Maximum(LunarOccultations.Elements.Count);
					((Control)pBar2).set_Visible(true);
					LunarOccultations.Prediction.Clear();
					for (int j = 0; j < LunarOccultations.Elements.Count; j++)
					{
						pBar2.set_Value(j);
						LunarOccultations.ComputeEventForALocation(j, longitude, CurrentSite.Latitude, CurrentSite.Altitude, pSin, pCos, CurrentSite.ApertureCM * 10f, CurrentSite.MagCorrection, CurrentSite.GrazeTravelDist, MultiSite: false, 0);
					}
					for (int num5 = 0; num5 < LunarOccultations.Prediction.Count; num5++)
					{
						using (StreamWriter streamWriter2 = new StreamWriter(path3, append: true))
						{
							streamWriter2.WriteLine(LunarOccultations.Prediction[num5].GrazeSummmaryLine());
						}
						LunarOccultations.GrazeSelectedStar = LunarOccultations.Prediction[num5].ElementRecordNumber;
						LunarOccultations.GrazeStartLongitude = Math.Floor(4.0 * CurrentSite.Longitude - 10.0) / 4.0;
						LunarOccultations.GrazeEndLongitude = Math.Floor(4.0 * CurrentSite.Longitude + 10.0) / 4.0;
						LunarOccultations.GrazeLongitudeStep = 0.25;
						LunarOccultations.GrazeNominalAltitude = CurrentSite.Altitude;
						LunarOccultations.GrazeLimit = 1;
						if ((LunarOccultations.Prediction[num5].AA > 90.0) & (LunarOccultations.Prediction[num5].AA < 270.0))
						{
							LunarOccultations.GrazeLimit = -1;
						}
						LunarOccultations.GrazePrediction(GetStar: false, cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
						text = $"{Year:F0}" + $"{Month:F0}".PadLeft(2, '0') + $"{day:F0}".PadLeft(2, '0') + " Graze of " + LunarOccultations.Prediction[num5].StarId.PadRight(7).Substring(0, 7).Trim();
						text2 = ((!Settings.Default.LunarNoSpacesInFileName) ? (path2 + "\\" + text + " " + text5) : (path2 + "\\" + (text + " " + text5).Replace(" ", "_")));
						if (chkIncludeText.get_Checked())
						{
							using StreamWriter streamWriter3 = new StreamWriter(text2 + ".txt", append: false);
							if ((LunarOccultations.GrazeHeader.Count < 1) | (LunarOccultations.GrazePredictionLines.Count < 1))
							{
								streamWriter3.WriteLine("No graze");
								break;
							}
							num = 1;
							for (int k = 0; k < LunarOccultations.GrazeHeader.Count; k++)
							{
								streamWriter3.WriteLine(LunarOccultations.GrazeHeader[k]!.ToString());
							}
							if (LunarOccultations.GrazePredictionLines[0].GrazeInnerOuterLimit == -1)
							{
								num = -1;
								streamWriter3.WriteLine("");
								streamWriter3.WriteLine(" Inner limit - " + LunarOccultations.GrazePredictionLines[0].StarId.Trim() + " fully obscured");
							}
							for (int k = 0; k < LunarOccultations.GrazePredictionLines.Count; k++)
							{
								if (LunarOccultations.GrazePredictionLines[k].GrazeInnerOuterLimit != num)
								{
									num = 1;
									streamWriter3.WriteLine("");
									streamWriter3.WriteLine(" Outer limit - " + LunarOccultations.GrazePredictionLines[0].StarId.Trim() + " just touches the moon");
								}
								streamWriter3.WriteLine(LunarOccultations.GrazePredictionLines[k].ToString());
							}
							for (int k = 0; k < LunarOccultations.GrazeFooter.Count; k++)
							{
								streamWriter3.WriteLine(LunarOccultations.GrazeFooter[k]!.ToString());
							}
							streamWriter3.WriteLine("Results of Observer Scan                                    UT");
							streamWriter3.WriteLine("Site                              Long.   Lat.   Dist.    h  m  s");
							for (int k = 0; k < LunarOccultations.GrazeSiteScan.Count; k++)
							{
								streamWriter3.WriteLine(LunarOccultations.GrazeSiteScan[k]!.ToString());
							}
							streamWriter3.WriteLine("");
							lstPrediction_Index.Add(-1);
							goto IL_0bdf;
						}
						goto IL_0bdf;
						IL_0bdf:
						if (chkIncludeProfile.get_Checked())
						{
							int librationDetails = LunarOccultations.GrazePredictionLines.Count / 2;
							if (LunarOccultations.GrazePredictionLines[0].GrazeInnerOuterLimit == -1)
							{
								librationDetails = LunarOccultations.GrazePredictionLines.Count / 4;
							}
							LunarProfile.SetLibrationDetails(librationDetails);
							LunarProfile.LibrationSetting = Settings.Default.GrazeProfile_LibrationPlot;
							LunarProfile.LimitToGrazes = Settings.Default.GrazeProfile_LimitToGrazes;
							LunarProfile.EventsInColour = Settings.Default.GrazeProfile_EventsInColour;
							LunarProfile.DrawInColor = optColor.get_Checked();
							LunarProfile.UseLOLAHires = chkUseHires.get_Checked() & optLOLA.get_Checked();
							LunarProfile.ShowLOLAPoints = chkUsePoints.get_Checked() & optLOLA.get_Checked();
							LunarProfile.generateHiresforEventCount = chkNumberOfEvents.get_Checked();
							LunarProfile.DistanceLines.Clear();
							if (optSmall.get_Checked())
							{
								LunarProfile.SaveGrazeProfile(640, 480, text2, ((ListControl)cmbFileType).get_SelectedIndex());
							}
							else if (optMedium.get_Checked())
							{
								LunarProfile.SaveGrazeProfile(800, 600, text2, ((ListControl)cmbFileType).get_SelectedIndex());
							}
							else
							{
								LunarProfile.SaveGrazeProfile(1024, 768, text2, ((ListControl)cmbFileType).get_SelectedIndex());
							}
						}
						if (chkIncludeKML.get_Checked())
						{
							LunarOccultations.MapSelectedStar = LunarOccultations.Prediction[num5].ElementRecordNumber;
							string text6 = LunarOccultations.EventID(LunarOccultations.MapSelectedStar);
							LunarOccultations.Compute_StartEnd_at_RiseSet_Curves();
							LunarOccultations.Compute_Limit_Lines();
							LunarOccultations.CreateGoogleEarthKMZFile_ForWorld(text2 + ".kml", "Occultation of " + text6, AutoOpenFile: true, View: false, "");
						}
						if (chkIncludeHTML.get_Checked())
						{
							string text7 = AppPath + "\\AutoGenerated Grazes\\GoogleMap files\\" + text.Replace(" ", "_") + ".htm";
							if (!File.Exists(text7))
							{
								LunarOccultations.MapSelectedStar = LunarOccultations.Prediction[num5].ElementRecordNumber;
								string text6 = LunarOccultations.EventID(LunarOccultations.GrazeSelectedStar);
								LunarOccultations.Compute_StartEnd_at_RiseSet_Curves();
								LunarOccultations.Compute_Limit_Lines();
								LunarOccultations.CreateGoogleMapHTMFile_ForWorld(text7, "Occultation of " + text6, AutoOpenFile: true);
							}
						}
						if (chkIncludeLocalHTML.get_Checked())
						{
							string text6 = LunarOccultations.EventID(LunarOccultations.MapSelectedStar);
							LunarOccultations.CreateGoogleMapHTMFile_ForGrazes(text2 + ".htm", "Graze of " + text6, AutoOpenFile: true);
						}
						if (chkRegional.get_Checked())
						{
							string path4 = AppPath + "\\AutoGenerated Grazes\\tmp\\" + text.Replace(" ", "_") + ".dat";
							if (!File.Exists(path4))
							{
								LunarOccultations.GrazeStartLongitude = Math.Floor(WestLimit - 4.0);
								LunarOccultations.GrazeEndLongitude = Math.Floor(EastLimit + 4.0);
								LunarOccultations.GrazeLongitudeStep = 1.0;
								LunarOccultations.GrazePrediction(GetStar: false, "");
								if (LunarOccultations.GrazePredictionLines.Count > 0)
								{
									using StreamWriter streamWriter4 = new StreamWriter(path4, append: false);
									streamWriter4.WriteLine(LunarOccultations.GrazePredictionLines[LunarOccultations.GrazePredictionLines.Count / 2].GrazeMapSummmaryLine());
									num = LunarOccultations.GrazePredictionLines[0].GrazeInnerOuterLimit;
									for (int k = 0; k < LunarOccultations.GrazePredictionLines.Count && LunarOccultations.GrazePredictionLines[k].GrazeInnerOuterLimit == num; k++)
									{
										if (LunarOccultations.GrazePredictionLines[k].MoonAlt > 0.0)
										{
											streamWriter4.WriteLine(string.Format("{0,10:F4}{1,10:F4}{2,5:F0}{3,5:F0}{4,5:F0}", LunarOccultations.GrazePredictionLines[k].GrazeLongitude, LunarOccultations.GrazePredictionLines[k].GrazeLatitude, LunarOccultations.GrazePredictionLines[k].CA, LunarOccultations.GrazePredictionLines[k].MoonAlt, LunarOccultations.GrazePredictionLines[k].SunAlt));
										}
									}
								}
							}
						}
					}
					((Control)pBar2).set_Visible(false);
				}
			}
			if (chkRegional.get_Checked())
			{
				SaveRegionalMap(768, 1024, AppPath + "\\AutoGenerated Grazes\\tmp\\Regional Grazes  - " + SiteFileName, ((ListControl)cmbFileType).get_SelectedIndex(), out SavedFileExtension);
				if (chkIncludeHTML.get_Checked())
				{
					CreateRegionalHTMLFile(AppPath + "\\AutoGenerated Grazes\\GoogleMap files\\RegionalGrazes_" + SiteFileName.Replace(" ", "_") + ".htm");
				}
			}
			string text8 = AppPath + "\\AutoGenerated Grazes\\_Predictions";
			for (int i = 0; i < ((ObjectCollection)lstSites.get_Items()).get_Count(); i++)
			{
				if (!lstSites.GetItemChecked(i))
				{
					continue;
				}
				CurrentSite = AllSites[i];
				string text5 = CurrentSite.Name.Replace(",", "").Replace("+", "_").Replace("/", "_");
				string path2 = AppPath + "\\AutoGenerated Grazes\\" + text5;
				if (chkRegional.get_Checked())
				{
					if (Settings.Default.LunarNoSpacesInFileName)
					{
						File.Copy(AppPath + "\\AutoGenerated Grazes\\tmp\\Regional Grazes  - " + SiteFileName + SavedFileExtension, path2 + "\\Regional_Grazes-" + SiteFileName.Replace(" ", "_") + SavedFileExtension, overwrite: true);
						File.Copy(AppPath + "\\AutoGenerated Grazes\\tmp\\Regional Grazes  - " + SiteFileName + ".txt", path2 + "\\Regional_Grazes-" + SiteFileName.Replace(" ", "_") + ".txt", overwrite: true);
					}
					else
					{
						File.Copy(AppPath + "\\AutoGenerated Grazes\\tmp\\Regional Grazes  - " + SiteFileName + SavedFileExtension, path2 + "\\Regional Grazes  - " + SiteFileName + SavedFileExtension, overwrite: true);
						File.Copy(AppPath + "\\AutoGenerated Grazes\\tmp\\Regional Grazes  - " + SiteFileName + ".txt", path2 + "\\Regional Grazes  - " + SiteFileName + ".txt", overwrite: true);
					}
				}
				if (!Directory.Exists(text8))
				{
					Directory.CreateDirectory(text8);
				}
				ZipFile.CreateFromDirectory(path2, text8 + "\\" + text5 + " " + text3 + " Grazes.zip");
				string[] files = Directory.GetFiles(path2, "*.txt");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.jpg");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.bmp");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.gif");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.png");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.tif");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.kml");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.kmz");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				files = Directory.GetFiles(path2, "*.htm");
				for (int l = 0; l < files.Length; l++)
				{
					File.Delete(files[l]);
				}
				File.Delete(path2 + "\\" + text4 + text5 + " Summary.txt");
				if (Directory.Exists(AppPath + "\\AutoGenerated Grazes\\tmp"))
				{
					files = Directory.GetFiles(AppPath + "\\AutoGenerated Grazes\\tmp", "*.dat");
					for (int l = 0; l < files.Length; l++)
					{
						File.Delete(files[l]);
					}
				}
			}
			((Control)cmdCancel).set_Visible(false);
		}

		private void CreateRegionalKMLFile(string FileName)
		{
			GoogleEarth.Create_New_GoogleEarthKMZ_File(FileName, "Regional Grazes", AutoOpenFile: true, out var CreatedFile);
			string[] files = Directory.GetFiles(AppPath + "\\AutoGenerated Grazes\\tmp", "*.txt");
			for (int i = 0; i < files.Length; i++)
			{
				using StreamReader streamReader = new StreamReader(files[i]);
				streamReader.ReadLine();
				GoogleEarth.Write_Tags_For_New_GoogleEarthKMZ_Path(1);
				while (!streamReader.EndOfStream)
				{
					string? text = streamReader.ReadLine();
					if (!double.TryParse(text!.Substring(0, 10), out var result))
					{
						result = 0.0;
					}
					if (!double.TryParse(text!.Substring(10, 10), out var result2))
					{
						result2 = 0.0;
					}
					GoogleEarth.Write_Path_Coordinate_GoogleEarthKMZ(result, result2, 0.0);
				}
				GoogleEarth.Write_Tags_At_End_of_Path_GoogleEarthKMZ();
			}
			CreatedFile = GoogleEarth.Close_GoogleEarthKMZ_File(CreatedFile, ConvertToKMZ: true);
		}

		private void CreateRegionalHTMLFile(string FileName)
		{
			double num = 0.0;
			double num2 = 0.0;
			double num3 = 0.0;
			double num4 = 0.0;
			int num5 = 0;
			GoogleEarth.Create_New_GoogleMap_File(FileName, "Regional Grazes", PositiveTimeIncrement: true, IncludeDirectionArrow: false, AutoOpenFile: true);
			string[] files = Directory.GetFiles(AppPath + "\\AutoGenerated Grazes\\tmp", "*.dat");
			for (int i = 0; i < files.Length; i++)
			{
				using StreamReader streamReader = new StreamReader(files[i]);
				streamReader.ReadLine();
				GoogleEarth.Write_Tags_For_New_GoogleMap_Polyline_Path();
				while (!streamReader.EndOfStream)
				{
					string? text = streamReader.ReadLine();
					if (!double.TryParse(text!.Substring(0, 10), out var result))
					{
						result = 0.0;
					}
					if (!double.TryParse(text!.Substring(10, 10), out var result2))
					{
						result2 = 0.0;
					}
					GoogleEarth.Write_PolyLine_Path_Coordinate_GoogleMap(result, result2, -1);
					if (num5 == 0)
					{
						num = result;
						num3 = result2;
					}
					num2 = result;
					num4 = result2;
				}
				GoogleEarth.Write_Tags_At_End_of_Polyline_Path_GoogleMap(-1);
			}
			double num6 = (num + num2) / 2.0;
			if (Math.Abs(num - num2) > 180.0)
			{
				num6 += 180.0;
			}
			if (num6 > 180.0)
			{
				num6 -= 360.0;
			}
			if (num6 < -180.0)
			{
				num6 += 360.0;
			}
			double centreLatitude = (num3 + num4) / 2.0;
			GoogleEarth.Close_GoogleMap_File(num6, centreLatitude);
		}

		internal void SaveRegionalMap(int Width, int Height, string OutputFile, int FileType, out string SavedFileExtension)
		{
			Application.DoEvents();
			SavedFileExtension = ".png";
			Bitmap bitmap = new Bitmap(Width, Height);
			Graphics graphics = Graphics.FromImage(bitmap);
			if (Settings.Default.GraphicsSmoothed)
			{
				graphics.SmoothingMode = SmoothingMode.AntiAlias;
			}
			graphics.Clear(Color.White);
			PlotRegion(graphics, Width, Height, Printer: false, OutputFile);
			switch (FileType)
			{
			case 0:
				SavedFileExtension = ".jpg";
				bitmap.Save(OutputFile + ".jpg", ImageFormat.Jpeg);
				break;
			case 1:
				SavedFileExtension = ".bmp";
				bitmap.Save(OutputFile + ".bmp", ImageFormat.Bmp);
				break;
			case 2:
				SavedFileExtension = ".gif";
				bitmap.Save(OutputFile + ".gif", ImageFormat.Gif);
				break;
			case 3:
				SavedFileExtension = ".png";
				bitmap.Save(OutputFile + ".png", ImageFormat.Png);
				break;
			case 4:
				SavedFileExtension = ".tif";
				bitmap.Save(OutputFile + ".tif", ImageFormat.Tiff);
				break;
			}
			graphics.Dispose();
		}

		private void PlotRegion(Graphics formGraphics, int ChartWidth, int ChartHeight, bool Printer, string LegendOutputFile)
		{
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			float y = 0f;
			float num4 = 0f;
			float y2 = 0f;
			bool flag = true;
			bool flag2 = true;
			bool flag3 = true;
			bool flag4 = true;
			Font font = new Font("Courier New", 8f);
			Font font2 = new Font("Courier New", 12f, FontStyle.Bold);
			Pen pen = new Pen(Brushes.Black, 0.5f);
			Pen pen2 = new Pen(Brushes.Black, 0.5f);
			Pen pen3 = new Pen(Brushes.Black, 0.5f);
			Pen pen4 = new Pen(Brushes.Black, 0.5f);
			Pen pen5 = new Pen(Brushes.Black, 1f);
			Pen pen6 = new Pen(Brushes.Black, 1f);
			Brush black = Brushes.Black;
			Brush black2 = Brushes.Black;
			pen2.DashPattern = new float[2] { 4f, 3f };
			pen3.DashPattern = new float[2] { 4f, 10f };
			pen4.DashPattern = new float[2] { 4f, 20f };
			if (Settings.Default.BWFlag || Printer)
			{
				formGraphics.Clear(Color.White);
			}
			else
			{
				formGraphics.Clear(Color.Black);
			}
			double westLimit = WestLimit;
			double num5 = EastLimit;
			double northLimit = NorthLimit;
			double southLimit = SouthLimit;
			if (westLimit > num5)
			{
				num5 += 360.0;
			}
			formGraphics.SmoothingMode = SmoothingMode.HighQuality;
			float num6 = 10f;
			float num7 = ChartWidth - 20;
			float num8 = 10f;
			float num9 = ChartHeight - 20;
			int num10 = 0;
			Maps.MapPlot(formGraphics, (float)ChartWidth / 2f, (float)ChartHeight / 2f, westLimit, num5, northLimit, southLimit, PlotCities: false, BWflag: true);
			using (StreamWriter streamWriter = new StreamWriter(LegendOutputFile + ".txt", append: false))
			{
				streamWriter.WriteLine(" #  Yr Mth Dy Hr Mn      Star  Mv   Mr    ill   Elon    Cusp");
				string[] files = Directory.GetFiles(AppPath + "\\AutoGenerated Grazes\\tmp", "*.dat");
				foreach (string path in files)
				{
					bool flag5 = true;
					bool flag6 = false;
					num10++;
					using (StreamReader streamReader = new StreamReader(path))
					{
						ArrayList arrayList = new ArrayList();
						streamWriter.WriteLine(string.Format("{0,2:F0}  ", num10) + streamReader.ReadLine());
						while (!streamReader.EndOfStream)
						{
							string? text = streamReader.ReadLine();
							if (!double.TryParse(text!.Substring(0, 10), out var result))
							{
								result = 0.0;
							}
							if (!double.TryParse(text!.Substring(10, 10), out var result2))
							{
								result2 = 0.0;
							}
							if (!double.TryParse(text!.Substring(30, 5), out var result3))
							{
								result3 = 0.0;
							}
							Maps.MapProjection(result, result2, out var x, out var y3);
							arrayList.Add(new PointF(x, y3));
							flag = text!.Substring(20, 5).Contains("-");
							flag3 = result3 > -5.0;
							if (!flag5 && ((flag != flag2 || flag3 != flag4) | streamReader.EndOfStream))
							{
								PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
								if (flag2)
								{
									if (flag4)
									{
										formGraphics.DrawLines(pen4, points);
									}
									else
									{
										formGraphics.DrawLines(pen2, points);
									}
								}
								else if (flag4)
								{
									formGraphics.DrawLines(pen3, points);
								}
								else
								{
									formGraphics.DrawLines(pen, points);
								}
								arrayList = new ArrayList();
								arrayList.Add(new PointF(x, y3));
							}
							flag5 = false;
							bool flag7 = x > num6 && x < num7 && y3 > num8 && y3 < num9;
							if (flag7 && !flag6)
							{
								num3 = x;
								y = y3;
							}
							if (flag6 & (!flag7 | streamReader.EndOfStream))
							{
								num4 = num;
								y2 = num2;
							}
							num = x;
							num2 = y3;
							flag4 = flag3;
							flag2 = flag;
							flag6 = flag7;
						}
					}
					num3 += (float)(-5 + 5 * (num10 % 3));
					num4 += (float)(5 - 5 * (num10 % 3));
					formGraphics.DrawString(num10.ToString(), font, black, num3, y);
					formGraphics.DrawString(num10.ToString(), font, black, num4, y2);
				}
			}
			formGraphics.DrawLine(pen, (float)ChartWidth / 20f, ChartHeight - 20, (float)(3 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Dark Limb", font2, black, (float)ChartWidth / 20f, ChartHeight - 15);
			formGraphics.DrawLine(pen2, (float)(5 * ChartWidth) / 20f, ChartHeight - 20, (float)(7 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Bright Limb", font2, black, (float)(5 * ChartWidth) / 20f, ChartHeight - 15);
			formGraphics.DrawLine(pen3, (float)(10 * ChartWidth) / 20f, ChartHeight - 20, (float)(12 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Dark, Sun>-5deg", font2, black, (float)(10 * ChartWidth) / 20f, ChartHeight - 15);
			formGraphics.DrawLine(pen4, (float)(15 * ChartWidth) / 20f, ChartHeight - 20, (float)(17 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Bright, Sun >-5deg", font2, black, (float)(15 * ChartWidth) / 20f, ChartHeight - 15);
			formGraphics.SmoothingMode = SmoothingMode.None;
			string path2 = AppPath + "\\Sites\\" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex());
			int num11 = 2;
			if (!File.Exists(path2))
			{
				return;
			}
			Sites sites = new Sites();
			float num12 = 0f;
			int num13 = 0;
			StreamReader streamReader2 = new StreamReader(path2);
			do
			{
				sites.Read_SiteFile(streamReader2.ReadLine());
				if (sites.PlotOnMap > num13)
				{
					Maps.MapProjection(sites.Longitude, sites.Latitude, out var x2, out var y4);
					if (sites.PlotOnMap <= 3)
					{
						num12 = sites.PlotOnMap switch
						{
							1 => 1f, 
							2 => 2f, 
							3 => 4f, 
							_ => 1f, 
						};
						formGraphics.DrawEllipse(pen5, x2 - num12, y4 - num12, 2f * num12, 2f * num12);
					}
					else if (sites.PlotOnMap > 3)
					{
						num12 = 1.5f;
						formGraphics.DrawRectangle(pen6, x2 - num12, y4 - num12, 2f * num12, 2f * num12);
					}
					if ((sites.PlotOnMap >= num11 || num11 == 1) && num11 > 0)
					{
						formGraphics.DrawString(sites.ShortName, font, black2, x2 + num12, y4 - 5f);
					}
				}
			}
			while (!streamReader2.EndOfStream);
			streamReader2.Close();
		}

		private void cmdComputeFixed_Click(object sender, EventArgs e)
		{
			string text = "";
			_ = new int[((ObjectCollection)lstSites.get_Items()).get_Count()];
			int num = 1;
			int selectedIndex = ((ListControl)cmbFileType).get_SelectedIndex();
			int num2 = 1;
			int num3 = 12;
			switch (((ListControl)cmbMonths).get_SelectedIndex())
			{
			case 0:
				num2 = 1;
				num3 = 12;
				break;
			case 1:
				num2 = 1;
				num3 = 6;
				break;
			case 2:
				num2 = 7;
				num3 = 12;
				break;
			case 3:
				num2 = 1;
				num3 = 3;
				break;
			case 4:
				num2 = 4;
				num3 = 6;
				break;
			case 5:
				num2 = 7;
				num3 = 9;
				break;
			case 6:
				num2 = 10;
				num3 = 12;
				break;
			default:
				num2 = 1;
				num3 = 12;
				break;
			}
			double num4 = Utilities.JD_from_Date((int)updnFixedYear.get_Value(), num2, 1.0);
			double num5 = Utilities.JD_from_Date((int)updnFixedYear.get_Value(), num3, 31.0);
			string path = AppPath + "\\AutoGenerated Grazes\\tmp";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			path = AppPath + "\\AutoGenerated Grazes\\World";
			if (!Directory.Exists(path))
			{
				Directory.CreateDirectory(path);
			}
			LunarOccultations.InitialiseSearch();
			LunarOccultations.InitialiseStarCatIndexArray(StarCat2);
			CancelFlag = false;
			((Control)cmdCancel).set_Visible(true);
			string.Concat($"{updnFixedYear.get_Value():F0}" + "0101 to " + $"{updnFixedYear.get_Value():F0}" + "1231", " Graze Summary for ");
			pBar2.set_Minimum(0);
			pBar2.set_Maximum(366);
			((Control)pBar2).set_Visible(true);
			int ChartWidth;
			int ChartHeight;
			for (double num6 = num4; num6 <= num5; num6 += 1.0)
			{
				pBar2.set_Value((int)(num6 - num4));
				((Control)lblCurrentDate).set_Text("[" + Utilities.Date_from_JD(num6, 0) + "]");
				Utilities.Date_from_JD(num6, out var Year, out var Month, out var day);
				Application.DoEvents();
				if (CancelFlag)
				{
					break;
				}
				LunarOccultations.NewDate(num6, 0, SiteSpecific: false, 90.0, -90.0, 180.0, -180.0);
				double readMagLimit = 4.0;
				LunarOccultations.ListGrazesOnly = true;
				LunarProfile.GetDataFiles();
				LunarOccultations.ScanXZFile(StarCat2, 90.0, -90.0, readMagLimit, DoublesOnly: false, Auto: true);
				for (int i = 0; i < LunarOccultations.Elements.Count; i++)
				{
					_ = LunarOccultations.Elements.Count;
					_ = 1;
					Application.DoEvents();
					if (CancelFlag)
					{
						break;
					}
					LunarOccultations.MapSelectedStar = i;
					LunarOccultations.GrazeSelectedStar = i;
					LunarOccultations.EventID(i);
					string text2 = LunarOccultations.Elements[i].StarId.PadRight(7).Substring(0, 7).Trim();
					if (!int.TryParse(text2.Substring(0, 2), out var _))
					{
						text2 = LunarOccultations.Elements[i].StarId.Trim();
					}
					switch (text2)
					{
					case "692":
						text2 = "Aldebaran";
						break;
					case "552":
						text2 = "Alcyone";
						break;
					case "810":
						text2 = "beta Tau";
						break;
					case "1487":
						text2 = "Regulus";
						break;
					case "1925":
						text2 = "Spica";
						break;
					case "2366":
						text2 = "Antares";
						break;
					}
					text = $"{Year:F0}" + $"{Month:F0}".PadLeft(2, '0') + $"{day:F0}".PadLeft(2, '0') + " Occultation of " + text2;
					string text3 = AppPath + "\\AutoGenerated Grazes\\World\\" + text;
					if ((LunarOccultations.Elements[i].Mv < 3.0) | ((LunarOccultations.Elements[i].PlanetDiaArcSec > 0.0) & (LunarOccultations.Elements[i].Mv < 7.0)))
					{
						ChartWidth = 800;
						ChartHeight = 600;
						Bitmap bitmap = new Bitmap(ChartWidth, ChartHeight);
						Graphics graphics = Graphics.FromImage(bitmap);
						if (Settings.Default.GraphicsSmoothed)
						{
							graphics.SmoothingMode = SmoothingMode.AntiAlias;
						}
						graphics.Clear(Color.White);
						LunarOccultations.DrawOccultationWorld(graphics, ChartWidth, ChartHeight, Printer: false, BWFlag: true, MultiPlot: false, 0);
						switch (((ListControl)cmbFileType).get_SelectedIndex())
						{
						case 0:
							bitmap.Save(text3 + ".jpg", ImageFormat.Jpeg);
							break;
						case 1:
							bitmap.Save(text3 + ".bmp", ImageFormat.Bmp);
							break;
						case 2:
							bitmap.Save(text3 + ".gif", ImageFormat.Gif);
							break;
						case 3:
							bitmap.Save(text3 + ".png", ImageFormat.Png);
							break;
						case 4:
							bitmap.Save(text3 + ".tif", ImageFormat.Tiff);
							break;
						}
						graphics.Dispose();
					}
					else
					{
						LunarOccultations.Compute_Limit_Lines();
						LunarOccultations.Compute_StartEnd_at_RiseSet_Curves();
					}
					for (int j = 0; j <= 1; j++)
					{
						if (LunarOccultations.LimitCount[j] <= 1)
						{
							continue;
						}
						LunarOccultations.GrazeLimit = 2 * j - 1;
						LunarOccultations.GrazeStartLongitude = Math.Floor(LunarOccultations.LimitLongitude[0, j] - 4.0);
						if (LunarOccultations.GrazeStartLongitude > 180.0)
						{
							LunarOccultations.GrazeStartLongitude -= 360.0;
						}
						LunarOccultations.GrazeEndLongitude = Math.Floor(LunarOccultations.LimitLongitude[LunarOccultations.LimitCount[j], j] + 4.0);
						if (LunarOccultations.GrazeEndLongitude > 180.0)
						{
							LunarOccultations.GrazeEndLongitude -= 360.0;
						}
						int num7 = LunarOccultations.LimitCount[j] / 2;
						if (num7 < 1)
						{
							num7 = 1;
						}
						if (LunarOccultations.LimitLongitude[num7 - 1, j] > LunarOccultations.LimitLongitude[num7, j])
						{
							Utilities.Swap(ref LunarOccultations.GrazeStartLongitude, ref LunarOccultations.GrazeEndLongitude);
						}
						if (LunarOccultations.GrazeStartLongitude > LunarOccultations.GrazeEndLongitude)
						{
							LunarOccultations.GrazeEndLongitude += 360.0;
						}
						LunarOccultations.GrazeLongitudeStep = 0.5;
						LunarOccultations.GrazeNominalAltitude = CurrentSite.Altitude;
						LunarOccultations.GrazePrediction(GetStar: false, "");
						string path2 = AppPath + "\\AutoGenerated Grazes\\tmp\\" + text.Replace(" ", "_") + "_" + "SN".Substring(j, 1) + ".dat";
						if (LunarOccultations.GrazePredictionLines.Count <= 0)
						{
							continue;
						}
						using StreamWriter streamWriter = new StreamWriter(path2, append: false);
						streamWriter.WriteLine(LunarOccultations.GrazePredictionLines[LunarOccultations.GrazePredictionLines.Count / 2].GrazeMapSummmaryLine());
						num = LunarOccultations.GrazePredictionLines[0].GrazeInnerOuterLimit;
						for (int k = 0; k < LunarOccultations.GrazePredictionLines.Count && LunarOccultations.GrazePredictionLines[k].GrazeInnerOuterLimit == num; k++)
						{
							if (LunarOccultations.GrazePredictionLines[k].MoonAlt > 0.0)
							{
								streamWriter.WriteLine(string.Format("{0,10:F4}{1,10:F4}{2,5:F0}{3,5:F0}{4,5:F0}", LunarOccultations.GrazePredictionLines[k].GrazeLongitude, LunarOccultations.GrazePredictionLines[k].GrazeLatitude, LunarOccultations.GrazePredictionLines[k].CA, LunarOccultations.GrazePredictionLines[k].MoonAlt, LunarOccultations.GrazePredictionLines[k].SunAlt));
							}
						}
					}
				}
			}
			for (int l = 0; l <= 6; l++)
			{
				if (GetFixedMapLimits(l, out var MapRegionName, out var WestLong, out var EastLong, out var NorthLat, out var SouthLat, out ChartWidth, out ChartHeight))
				{
					Bitmap bitmap2 = new Bitmap(ChartWidth, ChartHeight);
					Graphics graphics2 = Graphics.FromImage(bitmap2);
					string text4 = AppPath + "\\AutoGenerated Grazes\\World\\" + MapRegionName + " " + updnFixedYear.get_Value() + " " + cmbMonths.get_Items().get_Item(((ListControl)cmbMonths).get_SelectedIndex()).ToString();
					PlotFixedRegion(graphics2, WestLong, EastLong, NorthLat, SouthLat, ChartWidth, ChartHeight, Printer: false, text4);
					switch (selectedIndex)
					{
					case 0:
						bitmap2.Save(text4 + ".jpg", ImageFormat.Jpeg);
						break;
					case 1:
						bitmap2.Save(text4 + ".bmp", ImageFormat.Bmp);
						break;
					case 2:
						bitmap2.Save(text4 + ".gif", ImageFormat.Gif);
						break;
					case 3:
						bitmap2.Save(text4 + ".png", ImageFormat.Png);
						break;
					case 4:
						bitmap2.Save(text4 + ".tif", ImageFormat.Tiff);
						break;
					}
					graphics2.Dispose();
				}
			}
			string[] files = Directory.GetFiles(AppPath + "\\AutoGenerated Grazes\\tmp", "*.dat");
			for (int m = 0; m < files.Length; m++)
			{
				File.Delete(files[m]);
			}
			((Control)pBar2).set_Visible(false);
			CancelFlag = false;
			((Control)cmdCancel).set_Visible(false);
		}

		private void PlotFixedRegion(Graphics formGraphics, double WestLong, double EastLong, double NorthLat, double SouthLat, int ChartWidth, int ChartHeight, bool Printer, string LegendOutputFile)
		{
			float x = 0f;
			float y = 0f;
			float num = 0f;
			float num2 = 0f;
			float num3 = 0f;
			float y2 = 0f;
			float num4 = 0f;
			float y3 = 0f;
			double value = 0.0;
			bool flag = true;
			bool flag2 = true;
			bool flag3 = true;
			bool flag4 = true;
			bool flag5 = false;
			Font font = new Font("Courier New", 8f);
			Font font2 = new Font("Courier New", 12f, FontStyle.Bold);
			Pen pen = new Pen(Brushes.Black, 0.5f);
			Pen pen2 = new Pen(Brushes.Black, 0.5f);
			Pen pen3 = new Pen(Brushes.Black, 0.5f);
			Pen pen4 = new Pen(Brushes.Black, 0.5f);
			new Pen(Brushes.Black, 1f);
			new Pen(Brushes.Black, 1f);
			Brush black = Brushes.Black;
			_ = Brushes.Black;
			pen2.DashPattern = new float[2] { 4f, 3f };
			pen3.DashPattern = new float[2] { 4f, 10f };
			pen4.DashPattern = new float[2] { 4f, 20f };
			formGraphics.Clear(Color.White);
			formGraphics.SmoothingMode = SmoothingMode.HighQuality;
			float num5 = 10f;
			float num6 = ChartWidth - 20;
			float num7 = 10f;
			float num8 = ChartHeight - 20;
			int num9 = 0;
			double num10;
			if (EastLong - WestLong < 300.0)
			{
				Maps.MapPlot(formGraphics, (float)ChartWidth / 2f, (float)ChartHeight / 2f, WestLong, EastLong, NorthLat, SouthLat, PlotCities: false, BWflag: true);
				num10 = 4.0;
			}
			else
			{
				Maps.PlotMercatorWorld(formGraphics, ChartWidth, ChartHeight, 0f, BWflag: true, MultiPlot: false, 0);
				num10 = 2.0;
			}
			using (StreamWriter streamWriter = new StreamWriter(LegendOutputFile + ".txt", append: false))
			{
				streamWriter.WriteLine(" #  Yr Mth Dy Hr Mn      Star  Mv   Mr    ill   Elon    Cusp");
				string[] files = Directory.GetFiles(AppPath + "\\AutoGenerated Grazes\\tmp", "*.dat");
				foreach (string path in files)
				{
					bool flag6 = true;
					bool flag7 = false;
					flag5 = false;
					string text;
					using (StreamReader streamReader = new StreamReader(path))
					{
						ArrayList arrayList = new ArrayList();
						text = streamReader.ReadLine();
						if (!double.TryParse(text.Substring(26, 4), out var result))
						{
							result = 10.0;
						}
						if (result <= num10)
						{
							while (!streamReader.EndOfStream)
							{
								string? text2 = streamReader.ReadLine();
								if (!double.TryParse(text2!.Substring(0, 10), out var result2))
								{
									result2 = 0.0;
								}
								if (!double.TryParse(text2!.Substring(10, 10), out var result3))
								{
									result3 = 0.0;
								}
								if (!double.TryParse(text2!.Substring(30, 5), out var result4))
								{
									result4 = 0.0;
								}
								flag = text2!.Substring(20, 5).Contains("-");
								flag3 = result4 > -5.0;
								bool flag8;
								if (EastLong - WestLong < 300.0)
								{
									flag8 = Maps.MapProjection(result2, result3, out x, out y);
									flag8 = flag8 && x + 50f > num5 && x - 50f < num6 && y + 50f > num7 && y - 50f < num8;
								}
								else
								{
									Maps.MercatorXY((float)result2, (float)result3, ref x, ref y);
									flag8 = true;
									if (flag7)
									{
										flag8 = Math.Sign(result2) == Math.Sign(value);
									}
								}
								if (flag8)
								{
									arrayList.Add(new PointF(x, y));
								}
								if (!flag6 && ((flag != flag2 || flag3 != flag4 || (flag7 && !flag8)) | streamReader.EndOfStream))
								{
									if (arrayList.Count > 1)
									{
										PointF[] points = (PointF[])arrayList.ToArray(arrayList[0]!.GetType());
										if (flag2)
										{
											if (flag4)
											{
												formGraphics.DrawLines(pen4, points);
											}
											else
											{
												formGraphics.DrawLines(pen2, points);
											}
										}
										else if (flag4)
										{
											formGraphics.DrawLines(pen3, points);
										}
										else
										{
											formGraphics.DrawLines(pen, points);
										}
									}
									arrayList = new ArrayList();
									if (flag7 && !flag8)
									{
										flag6 = true;
									}
									else
									{
										arrayList.Add(new PointF(x, y));
									}
								}
								flag6 = false;
								flag8 = x > num5 && x < num6 && y > num7 && y < num8 - 10f;
								if (flag8 && !flag7)
								{
									num3 = x;
									y2 = y;
									flag5 = true;
								}
								if (flag7 & (!flag8 | streamReader.EndOfStream))
								{
									num4 = num;
									y3 = num2;
									flag5 = true;
								}
								num = x;
								num2 = y;
								flag4 = flag3;
								flag2 = flag;
								value = result2;
								flag7 = flag8;
							}
						}
					}
					if (flag5)
					{
						num9++;
						streamWriter.WriteLine(string.Format("{0,2:F0}  ", num9) + text);
						num3 += (float)(-5 + 5 * (num9 % 3));
						num4 += (float)(5 - 5 * (num9 % 3));
						formGraphics.DrawString(num9.ToString(), font, black, num3, y2);
						formGraphics.DrawString(num9.ToString(), font, black, num4, y3);
					}
				}
			}
			formGraphics.DrawLine(pen, (float)ChartWidth / 20f, ChartHeight - 20, (float)(3 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Dark Limb", font2, black, (float)ChartWidth / 20f, ChartHeight - 15);
			formGraphics.DrawLine(pen2, (float)(5 * ChartWidth) / 20f, ChartHeight - 20, (float)(7 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Bright Limb", font2, black, (float)(5 * ChartWidth) / 20f, ChartHeight - 15);
			formGraphics.DrawLine(pen3, (float)(10 * ChartWidth) / 20f, ChartHeight - 20, (float)(12 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Dark, Sun>-5deg", font2, black, (float)(10 * ChartWidth) / 20f, ChartHeight - 15);
			formGraphics.DrawLine(pen4, (float)(15 * ChartWidth) / 20f, ChartHeight - 20, (float)(17 * ChartWidth) / 20f, ChartHeight - 20);
			formGraphics.DrawString("Bright, Sun >-5deg", font2, black, (float)(15 * ChartWidth) / 20f, ChartHeight - 15);
			formGraphics.SmoothingMode = SmoothingMode.None;
		}

		private static bool GetFixedMapLimits(int MapRegion, out string MapRegionName, out double WestLong, out double EastLong, out double NorthLat, out double SouthLat, out int ChartWidth, out int ChartHeight)
		{
			EastLong = (WestLong = (NorthLat = (SouthLat = 0.0)));
			MapRegionName = "";
			switch (MapRegion)
			{
			case 0:
				MapRegionName = Settings.Default.GMapName0;
				WestLong = (double)Settings.Default.GMapW0;
				EastLong = (double)Settings.Default.GMapE0;
				NorthLat = (double)Settings.Default.GMapN0;
				SouthLat = (double)Settings.Default.GMapS0;
				break;
			case 1:
				MapRegionName = Settings.Default.GMapName1;
				WestLong = (double)Settings.Default.GMapW1;
				EastLong = (double)Settings.Default.GMapE1;
				NorthLat = (double)Settings.Default.GMapN1;
				SouthLat = (double)Settings.Default.GMapS1;
				break;
			case 2:
				MapRegionName = Settings.Default.GMapName2;
				WestLong = (double)Settings.Default.GMapW2;
				EastLong = (double)Settings.Default.GMapE2;
				NorthLat = (double)Settings.Default.GMapN2;
				SouthLat = (double)Settings.Default.GMapS2;
				break;
			case 3:
				MapRegionName = Settings.Default.GMapName3;
				WestLong = (double)Settings.Default.GMapW3;
				EastLong = (double)Settings.Default.GMapE3;
				NorthLat = (double)Settings.Default.GMapN3;
				SouthLat = (double)Settings.Default.GMapS3;
				break;
			case 4:
				MapRegionName = Settings.Default.GMapName4;
				WestLong = (double)Settings.Default.GMapW4;
				EastLong = (double)Settings.Default.GMapE4;
				NorthLat = (double)Settings.Default.GMapN4;
				SouthLat = (double)Settings.Default.GMapS4;
				break;
			case 5:
				MapRegionName = Settings.Default.GMapName5;
				WestLong = (double)Settings.Default.GMapW5;
				EastLong = (double)Settings.Default.GMapE5;
				NorthLat = (double)Settings.Default.GMapN5;
				SouthLat = (double)Settings.Default.GMapS5;
				break;
			case 6:
				MapRegionName = Settings.Default.GMapName6;
				WestLong = (double)Settings.Default.GMapW6;
				EastLong = (double)Settings.Default.GMapE6;
				NorthLat = (double)Settings.Default.GMapN6;
				SouthLat = (double)Settings.Default.GMapS6;
				break;
			}
			ChartWidth = 1000;
			if (MapRegionName.Length < 1)
			{
				MapRegionName = MapRegion.ToString();
			}
			if (EastLong != WestLong)
			{
				ChartHeight = (int)((double)ChartWidth / (EastLong - WestLong) * (NorthLat - SouthLat));
				return true;
			}
			ChartHeight = 500;
			return false;
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			CancelFlag = true;
		}

		private void cmbLibrations_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.GrazeProfile_LibrationPlot = ((ListControl)cmbLibrations).get_SelectedIndex();
		}

		private void SetSaveSizeButtons()
		{
			Settings.Default.GrazeProfileSaveSmall = optSmall.get_Checked();
			Settings.Default.GrazeProfileSaveMedium = optMedium.get_Checked();
			Settings.Default.GrazeProfileSaveLarge = optLarge.get_Checked();
		}

		private void optSmall_Click(object sender, EventArgs e)
		{
			SetSaveSizeButtons();
		}

		private void optMedium_Click(object sender, EventArgs e)
		{
			SetSaveSizeButtons();
		}

		private void optLarge_Click(object sender, EventArgs e)
		{
			SetSaveSizeButtons();
		}

		private void cmdFullYear_Click(object sender, EventArgs e)
		{
			((ListControl)cmbLunarMonthStart).set_SelectedIndex(0);
			((ListControl)cmbLunarDayStart).set_SelectedIndex(0);
			updnLunarYearEnd.set_Value(updnLunarYearStart.get_Value());
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(11);
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(30);
		}

		private void cmdFullMonth_Click(object sender, EventArgs e)
		{
			((ListControl)cmbLunarDayStart).set_SelectedIndex(0);
			updnLunarYearEnd.set_Value(updnLunarYearStart.get_Value());
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(((ListControl)cmbLunarMonthStart).get_SelectedIndex());
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(cmbLunarDayEnd.get_Items().get_Count() - 1);
		}

		private void cmdOneDay_Click(object sender, EventArgs e)
		{
			updnLunarYearEnd.set_Value(updnLunarYearStart.get_Value());
			((ListControl)cmbLunarMonthEnd).set_SelectedIndex(((ListControl)cmbLunarMonthStart).get_SelectedIndex());
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(((ListControl)cmbLunarDayStart).get_SelectedIndex());
		}

		private void button1_Click(object sender, EventArgs e)
		{
			DateTime now = DateTime.Now;
			updnLunarYearStart.set_Value((decimal)now.ToUniversalTime().Year);
			((ListControl)cmbLunarMonthStart).set_SelectedIndex(now.ToUniversalTime().Month - 1);
			((ListControl)cmbLunarDayStart).set_SelectedIndex(now.ToUniversalTime().Day - 1);
		}

		private void cmd3Mths_Click(object sender, EventArgs e)
		{
			((ListControl)cmbLunarDayStart).set_SelectedIndex(0);
			if (((ListControl)cmbLunarMonthStart).get_SelectedIndex() < 10)
			{
				((ListControl)cmbLunarMonthEnd).set_SelectedIndex(((ListControl)cmbLunarMonthStart).get_SelectedIndex() + 2);
			}
			else
			{
				updnLunarYearEnd.set_Value(updnLunarYearStart.get_Value() + 1m);
				((ListControl)cmbLunarMonthEnd).set_SelectedIndex(((ListControl)cmbLunarMonthStart).get_SelectedIndex() - 10);
			}
			((ListControl)cmbLunarDayEnd).set_SelectedIndex(cmbLunarDayEnd.get_Items().get_Count() - 1);
		}

		private void LunarAutoGenerate_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Lunar Occultations - Auto generate");
		}

		private void chkShortOutput_CheckedChanged(object sender, EventArgs e)
		{
			//IL_0033: Unknown result type (might be due to invalid IL or missing references)
			if (chkShortOutput.get_Checked() && (chkAsteroids.get_Checked() || !optZC.get_Checked()))
			{
				MessageBox.Show("Check star catalogue selection.\r\n\r\n 'ZC' is recommended for Short output.\r\n\r\nSelect Stars and Planets, but NOT Asteroids", "Confirm star catalogue");
			}
		}

		private void cmbLimitingMagnitude_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.LunarMultipleMagLimit = ((ListControl)cmbLimitingMagnitude).get_SelectedIndex();
		}

		private void cmdMergeBAA_Click(object sender, EventArgs e)
		{
			//IL_0005: Unknown result type (might be due to invalid IL or missing references)
			((Form)new BAAMerge()).ShowDialog();
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
			//IL_060e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0618: Expected O, but got Unknown
			//IL_06c0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ca: Expected O, but got Unknown
			//IL_0772: Unknown result type (might be due to invalid IL or missing references)
			//IL_077c: Expected O, but got Unknown
			//IL_1836: Unknown result type (might be due to invalid IL or missing references)
			//IL_1840: Expected O, but got Unknown
			//IL_1caf: Unknown result type (might be due to invalid IL or missing references)
			//IL_1cb9: Expected O, but got Unknown
			//IL_1d8e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1d98: Expected O, but got Unknown
			//IL_247d: Unknown result type (might be due to invalid IL or missing references)
			//IL_2487: Expected O, but got Unknown
			//IL_3809: Unknown result type (might be due to invalid IL or missing references)
			//IL_3813: Expected O, but got Unknown
			//IL_3b47: Unknown result type (might be due to invalid IL or missing references)
			//IL_3b51: Expected O, but got Unknown
			//IL_3dad: Unknown result type (might be due to invalid IL or missing references)
			//IL_3db7: Expected O, but got Unknown
			//IL_3e71: Unknown result type (might be due to invalid IL or missing references)
			//IL_3e7b: Expected O, but got Unknown
			//IL_3f47: Unknown result type (might be due to invalid IL or missing references)
			//IL_3f51: Expected O, but got Unknown
			//IL_400b: Unknown result type (might be due to invalid IL or missing references)
			//IL_4015: Expected O, but got Unknown
			//IL_40de: Unknown result type (might be due to invalid IL or missing references)
			//IL_40e8: Expected O, but got Unknown
			//IL_41a2: Unknown result type (might be due to invalid IL or missing references)
			//IL_41ac: Expected O, but got Unknown
			//IL_46ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_46d4: Expected O, but got Unknown
			//IL_4a09: Unknown result type (might be due to invalid IL or missing references)
			//IL_4a13: Expected O, but got Unknown
			//IL_4c47: Unknown result type (might be due to invalid IL or missing references)
			//IL_4c51: Expected O, but got Unknown
			//IL_4cb8: Unknown result type (might be due to invalid IL or missing references)
			//IL_4cc2: Expected O, but got Unknown
			grpObjects = new GroupBox();
			chkAsteroids = new CheckBox();
			chkStars = new CheckBox();
			chkPlanets = new CheckBox();
			grpXZ80 = new GroupBox();
			optXZ9 = new RadioButton();
			optXZ6 = new RadioButton();
			optXZ3 = new RadioButton();
			optZC = new RadioButton();
			optXZ = new RadioButton();
			cmbSiteFiles = new ComboBox();
			label28 = new Label();
			cmbLunarDayEnd = new ComboBox();
			cmbLunarDayStart = new ComboBox();
			cmbLunarMonthEnd = new ComboBox();
			cmbLunarMonthStart = new ComboBox();
			updnLunarYearEnd = new NumericUpDown();
			updnLunarYearStart = new NumericUpDown();
			labelIntegration = new Label();
			cmdOccultationsForAll = new Button();
			cmdGrazes = new Button();
			cmdCancel = new Button();
			pBar2 = new ProgressBar();
			pBar = new ProgressBar();
			lblCurrentDate = new Label();
			lstSites = new CheckedListBox();
			cmdAll = new Button();
			cmdNone = new Button();
			label1 = new Label();
			grpOutputs = new GroupBox();
			label20 = new Label();
			label19 = new Label();
			label14 = new Label();
			chkZip = new CheckBox();
			chkDelete = new CheckBox();
			label13 = new Label();
			label8 = new Label();
			label50 = new Label();
			cmbLibrations = new ComboBox();
			label3 = new Label();
			cmbFileType = new ComboBox();
			label2 = new Label();
			groupBox1 = new GroupBox();
			lblRange = new Label();
			chkFilter = new CheckBox();
			lblCount = new Label();
			groupBox2 = new GroupBox();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			optLarge = new RadioButton();
			optMedium = new RadioButton();
			optSmall = new RadioButton();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			button1 = new Button();
			cmdFullMonth = new Button();
			cmdOneDay = new Button();
			cmdFullYear = new Button();
			groupBox3 = new GroupBox();
			label18 = new Label();
			cmd3Mths = new Button();
			groupBox4 = new GroupBox();
			panel2 = new Panel();
			label24 = new Label();
			optBandW = new RadioButton();
			optColor = new RadioButton();
			panel1 = new Panel();
			optLOLA = new RadioButton();
			chkNumberOfEvents = new CheckBox();
			label22 = new Label();
			chkUsePoints = new CheckBox();
			chkUseHires = new CheckBox();
			chkUseLoRes = new CheckBox();
			chkSpaces = new CheckBox();
			label17 = new Label();
			label16 = new Label();
			label15 = new Label();
			chkRegional = new CheckBox();
			chkIncludeLocalHTML = new CheckBox();
			chkIncludeText = new CheckBox();
			chkIncludeHTML = new CheckBox();
			chkIncludeProfile = new CheckBox();
			chkIncludeKML = new CheckBox();
			groupBox5 = new GroupBox();
			cmbMonths = new ComboBox();
			cmdComputeFixed = new Button();
			label21 = new Label();
			updnFixedYear = new NumericUpDown();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			chkShortOutput = new CheckBox();
			cmbLimitingMagnitude = new ComboBox();
			label23 = new Label();
			groupBox6 = new GroupBox();
			chkUseCA = new CheckBox();
			cmdMergeBAA = new Button();
			((Control)grpObjects).SuspendLayout();
			((Control)grpXZ80).SuspendLayout();
			((ISupportInitialize)updnLunarYearEnd).BeginInit();
			((ISupportInitialize)updnLunarYearStart).BeginInit();
			((Control)grpOutputs).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)groupBox2).SuspendLayout();
			((Control)groupBox3).SuspendLayout();
			((Control)groupBox4).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)groupBox5).SuspendLayout();
			((ISupportInitialize)updnFixedYear).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)groupBox6).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)grpObjects).get_Controls().Add((Control)(object)chkAsteroids);
			((Control)grpObjects).get_Controls().Add((Control)(object)chkStars);
			((Control)grpObjects).get_Controls().Add((Control)(object)chkPlanets);
			((Control)grpObjects).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)grpObjects).set_Location(new Point(218, 25));
			((Control)grpObjects).set_Name("grpObjects");
			((Control)grpObjects).set_Size(new Size(94, 68));
			((Control)grpObjects).set_TabIndex(76);
			grpObjects.set_TabStop(false);
			((Control)grpObjects).set_Text("Objects");
			((Control)chkAsteroids).set_AutoSize(true);
			chkAsteroids.set_Checked(Settings.Default.Lunar_IncludeAsteroids);
			chkAsteroids.set_CheckState((CheckState)1);
			((Control)chkAsteroids).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Lunar_IncludeAsteroids", true, (DataSourceUpdateMode)1));
			((Control)chkAsteroids).set_Location(new Point(5, 50));
			((Control)chkAsteroids).set_Name("chkAsteroids");
			((Control)chkAsteroids).set_Size(new Size(69, 17));
			((Control)chkAsteroids).set_TabIndex(2);
			((Control)chkAsteroids).set_Text("Asteroids");
			((ButtonBase)chkAsteroids).set_UseVisualStyleBackColor(true);
			((Control)chkStars).set_AutoSize(true);
			chkStars.set_Checked(Settings.Default.Lunar_IncludeStars);
			chkStars.set_CheckState((CheckState)1);
			((Control)chkStars).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Lunar_IncludeStars", true, (DataSourceUpdateMode)1));
			((Control)chkStars).set_Location(new Point(5, 16));
			((Control)chkStars).set_Name("chkStars");
			((Control)chkStars).set_Size(new Size(50, 17));
			((Control)chkStars).set_TabIndex(0);
			((Control)chkStars).set_Text("Stars");
			((ButtonBase)chkStars).set_UseVisualStyleBackColor(true);
			((Control)chkPlanets).set_AutoSize(true);
			chkPlanets.set_Checked(Settings.Default.Lunar_IncludePlanets);
			chkPlanets.set_CheckState((CheckState)1);
			((Control)chkPlanets).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Lunar_IncludePlanets", true, (DataSourceUpdateMode)1));
			((Control)chkPlanets).set_Location(new Point(5, 33));
			((Control)chkPlanets).set_Name("chkPlanets");
			((Control)chkPlanets).set_Size(new Size(61, 17));
			((Control)chkPlanets).set_TabIndex(1);
			((Control)chkPlanets).set_Text("Planets");
			((ButtonBase)chkPlanets).set_UseVisualStyleBackColor(true);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ9);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ6);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ3);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optZC);
			((Control)grpXZ80).get_Controls().Add((Control)(object)optXZ);
			((Control)grpXZ80).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)grpXZ80).set_Location(new Point(62, 25));
			((Control)grpXZ80).set_Name("grpXZ80");
			((Control)grpXZ80).set_Size(new Size(94, 127));
			((Control)grpXZ80).set_TabIndex(75);
			grpXZ80.set_TabStop(false);
			((Control)grpXZ80).set_Text("Star catalogue");
			((Control)optXZ9).set_AutoSize(true);
			optXZ9.set_Checked(true);
			((Control)optXZ9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optXZ9).set_Location(new Point(7, 58));
			((Control)optXZ9).set_Name("optXZ9");
			((Control)optXZ9).set_Size(new Size(83, 17));
			((Control)optXZ9).set_TabIndex(4);
			optXZ9.set_TabStop(true);
			((Control)optXZ9).set_Text("XZ  < mag 9");
			((ButtonBase)optXZ9).set_UseVisualStyleBackColor(true);
			optXZ9.add_CheckedChanged((EventHandler)optXZ9_CheckedChanged);
			((Control)optXZ6).set_AutoSize(true);
			((Control)optXZ6).set_Location(new Point(7, 78));
			((Control)optXZ6).set_Name("optXZ6");
			((Control)optXZ6).set_Size(new Size(83, 17));
			((Control)optXZ6).set_TabIndex(3);
			((Control)optXZ6).set_Text("XZ  < mag 7");
			((ButtonBase)optXZ6).set_UseVisualStyleBackColor(true);
			optXZ6.add_CheckedChanged((EventHandler)optXZ6_CheckedChanged);
			((Control)optXZ3).set_AutoSize(true);
			((Control)optXZ3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optXZ3).set_Location(new Point(7, 98));
			((Control)optXZ3).set_Name("optXZ3");
			((Control)optXZ3).set_Size(new Size(83, 17));
			((Control)optXZ3).set_TabIndex(2);
			((Control)optXZ3).set_Text("XZ  < mag 4");
			((ButtonBase)optXZ3).set_UseVisualStyleBackColor(true);
			optXZ3.add_CheckedChanged((EventHandler)optXZ3_CheckedChanged);
			((Control)optZC).set_AutoSize(true);
			((Control)optZC).set_Location(new Point(7, 38));
			((Control)optZC).set_Name("optZC");
			((Control)optZC).set_Size(new Size(39, 17));
			((Control)optZC).set_TabIndex(1);
			((Control)optZC).set_Text("ZC");
			((ButtonBase)optZC).set_UseVisualStyleBackColor(true);
			optZC.add_CheckedChanged((EventHandler)optZC_CheckedChanged);
			((Control)optXZ).set_AutoSize(true);
			((Control)optXZ).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optXZ).set_Location(new Point(7, 18));
			((Control)optXZ).set_Name("optXZ");
			((Control)optXZ).set_Size(new Size(41, 17));
			((Control)optXZ).set_TabIndex(0);
			((Control)optXZ).set_Text("XZ");
			((ButtonBase)optXZ).set_UseVisualStyleBackColor(true);
			optXZ.add_CheckedChanged((EventHandler)optXZ_CheckedChanged);
			((Control)cmbSiteFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(16, 19));
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(132, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(73);
			cmbSiteFiles.add_SelectedIndexChanged((EventHandler)cmbSiteFiles_SelectedIndexChanged);
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(8, 61));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(51, 13));
			((Control)label28).set_TabIndex(69);
			((Control)label28).set_Text("Last date");
			cmbLunarDayEnd.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLunarDayEnd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLunarDayEnd).set_FormattingEnabled(true);
			cmbLunarDayEnd.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbLunarDayEnd).set_Location(new Point(157, 59));
			cmbLunarDayEnd.set_MaxDropDownItems(31);
			((Control)cmbLunarDayEnd).set_Name("cmbLunarDayEnd");
			((Control)cmbLunarDayEnd).set_Size(new Size(39, 21));
			((Control)cmbLunarDayEnd).set_TabIndex(72);
			cmbLunarDayEnd.add_SelectedIndexChanged((EventHandler)cmbLunarDayEnd_SelectedIndexChanged);
			cmbLunarDayStart.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLunarDayStart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLunarDayStart).set_FormattingEnabled(true);
			cmbLunarDayStart.get_Items().AddRange(new object[31]
			{
				"1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
				"11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
				"21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
				"31"
			});
			((Control)cmbLunarDayStart).set_Location(new Point(156, 30));
			cmbLunarDayStart.set_MaxDropDownItems(31);
			((Control)cmbLunarDayStart).set_Name("cmbLunarDayStart");
			((Control)cmbLunarDayStart).set_Size(new Size(40, 21));
			((Control)cmbLunarDayStart).set_TabIndex(68);
			cmbLunarDayStart.add_SelectedIndexChanged((EventHandler)cmbLunarDayStart_SelectedIndexChanged);
			cmbLunarMonthEnd.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLunarMonthEnd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLunarMonthEnd).set_FormattingEnabled(true);
			cmbLunarMonthEnd.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbLunarMonthEnd).set_Location(new Point(113, 59));
			cmbLunarMonthEnd.set_MaxDropDownItems(12);
			((Control)cmbLunarMonthEnd).set_Name("cmbLunarMonthEnd");
			((Control)cmbLunarMonthEnd).set_Size(new Size(44, 21));
			((Control)cmbLunarMonthEnd).set_TabIndex(71);
			cmbLunarMonthEnd.add_SelectedIndexChanged((EventHandler)cmbLunarMonthEnd_SelectedIndexChanged);
			cmbLunarMonthStart.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLunarMonthStart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLunarMonthStart).set_FormattingEnabled(true);
			cmbLunarMonthStart.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbLunarMonthStart).set_Location(new Point(113, 30));
			cmbLunarMonthStart.set_MaxDropDownItems(12);
			((Control)cmbLunarMonthStart).set_Name("cmbLunarMonthStart");
			((Control)cmbLunarMonthStart).set_Size(new Size(43, 21));
			((Control)cmbLunarMonthStart).set_TabIndex(67);
			cmbLunarMonthStart.add_SelectedIndexChanged((EventHandler)cmbLunarMonthStart_SelectedIndexChanged);
			((Control)updnLunarYearEnd).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnLunarYearEnd).set_Location(new Point(62, 60));
			updnLunarYearEnd.set_Maximum(new decimal(new int[4] { 2200, 0, 0, 0 }));
			updnLunarYearEnd.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnLunarYearEnd).set_Name("updnLunarYearEnd");
			((Control)updnLunarYearEnd).set_Size(new Size(51, 20));
			((Control)updnLunarYearEnd).set_TabIndex(70);
			updnLunarYearEnd.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			updnLunarYearEnd.add_ValueChanged((EventHandler)updnLunarYearEnd_ValueChanged);
			((Control)updnLunarYearStart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnLunarYearStart).set_Location(new Point(62, 31));
			updnLunarYearStart.set_Maximum(new decimal(new int[4] { 9999, 0, 0, 0 }));
			updnLunarYearStart.set_Minimum(new decimal(new int[4] { 5000, 0, 0, -2147483648 }));
			((Control)updnLunarYearStart).set_Name("updnLunarYearStart");
			((Control)updnLunarYearStart).set_Size(new Size(51, 20));
			((Control)updnLunarYearStart).set_TabIndex(66);
			updnLunarYearStart.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			updnLunarYearStart.add_ValueChanged((EventHandler)updnLunarYearStart_ValueChanged);
			((Control)labelIntegration).set_AutoSize(true);
			((Control)labelIntegration).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)labelIntegration).set_Location(new Point(215, 98));
			((Control)labelIntegration).set_Name("labelIntegration");
			((Control)labelIntegration).set_Size(new Size(90, 26));
			((Control)labelIntegration).set_TabIndex(77);
			((Control)labelIntegration).set_Text("Asteroid elements\r\nnot integrated");
			((Control)labelIntegration).set_Visible(false);
			((Control)cmdOccultationsForAll).set_Font(new Font("Microsoft Sans Serif", 10.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdOccultationsForAll).set_Location(new Point(19, 13));
			((Control)cmdOccultationsForAll).set_Name("cmdOccultationsForAll");
			((Control)cmdOccultationsForAll).set_Size(new Size(110, 58));
			((Control)cmdOccultationsForAll).set_TabIndex(80);
			((Control)cmdOccultationsForAll).set_Text("Compute\r\nOccultations");
			((ButtonBase)cmdOccultationsForAll).set_UseVisualStyleBackColor(true);
			((Control)cmdOccultationsForAll).add_Click((EventHandler)cmdOccultationsForAll_Click);
			((Control)cmdGrazes).set_Font(new Font("Microsoft Sans Serif", 10.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdGrazes).set_Location(new Point(607, 228));
			((Control)cmdGrazes).set_Name("cmdGrazes");
			((Control)cmdGrazes).set_Size(new Size(110, 58));
			((Control)cmdGrazes).set_TabIndex(82);
			((Control)cmdGrazes).set_Text("Compute Grazes");
			((ButtonBase)cmdGrazes).set_UseVisualStyleBackColor(true);
			((Control)cmdGrazes).add_Click((EventHandler)cmdGrazes_Click);
			cmdCancel.set_DialogResult((DialogResult)2);
			((Control)cmdCancel).set_Location(new Point(798, 168));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(68, 41));
			((Control)cmdCancel).set_TabIndex(83);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).set_Visible(false);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)pBar2).set_Location(new Point(609, 183));
			((Control)pBar2).set_Name("pBar2");
			((Control)pBar2).set_Size(new Size(150, 10));
			((Control)pBar2).set_TabIndex(85);
			((Control)pBar2).set_Visible(false);
			((Control)pBar).set_Location(new Point(609, 172));
			((Control)pBar).set_Name("pBar");
			((Control)pBar).set_Size(new Size(150, 10));
			((Control)pBar).set_TabIndex(84);
			((Control)pBar).set_Visible(false);
			((Control)lblCurrentDate).set_AutoSize(true);
			((Control)lblCurrentDate).set_Location(new Point(648, 156));
			((Control)lblCurrentDate).set_Name("lblCurrentDate");
			((Control)lblCurrentDate).set_Size(new Size(13, 13));
			((Control)lblCurrentDate).set_TabIndex(86);
			((Control)lblCurrentDate).set_Text("[]");
			lstSites.set_CheckOnClick(true);
			((Control)lstSites).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSites).set_FormattingEnabled(true);
			((Control)lstSites).set_Location(new Point(10, 108));
			((Control)lstSites).set_Name("lstSites");
			((Control)lstSites).set_Size(new Size(145, 364));
			((Control)lstSites).set_TabIndex(89);
			((Control)lstSites).add_MouseUp(new MouseEventHandler(lstSites_MouseUp));
			((Control)cmdAll).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdAll).set_Location(new Point(25, 490));
			((Control)cmdAll).set_Name("cmdAll");
			((Control)cmdAll).set_Size(new Size(49, 37));
			((Control)cmdAll).set_TabIndex(90);
			((Control)cmdAll).set_Text("Check All");
			((ButtonBase)cmdAll).set_UseVisualStyleBackColor(true);
			((Control)cmdAll).add_Click((EventHandler)cmdAll_Click);
			((Control)cmdNone).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdNone).set_Location(new Point(90, 490));
			((Control)cmdNone).set_Name("cmdNone");
			((Control)cmdNone).set_Size(new Size(49, 37));
			((Control)cmdNone).set_TabIndex(91);
			((Control)cmdNone).set_Text("Clear All");
			((ButtonBase)cmdNone).set_UseVisualStyleBackColor(true);
			((Control)cmdNone).add_Click((EventHandler)cmdNone_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(36, 79));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(91, 26));
			((Control)label1).set_TabIndex(92);
			((Control)label1).set_Text("Select sites for\r\nthe prediction");
			((Control)grpOutputs).get_Controls().Add((Control)(object)label20);
			((Control)grpOutputs).get_Controls().Add((Control)(object)label19);
			((Control)grpOutputs).get_Controls().Add((Control)(object)label14);
			((Control)grpOutputs).get_Controls().Add((Control)(object)chkZip);
			((Control)grpOutputs).get_Controls().Add((Control)(object)chkDelete);
			((Control)grpOutputs).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpOutputs).set_Location(new Point(186, 473));
			((Control)grpOutputs).set_Name("grpOutputs");
			((Control)grpOutputs).set_Size(new Size(375, 103));
			((Control)grpOutputs).set_TabIndex(94);
			grpOutputs.set_TabStop(false);
			((Control)grpOutputs).set_Text("4.  Set output options");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(182, 47));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(192, 39));
			((Control)label20).set_TabIndex(108);
			((Control)label20).set_Text("All output is zipped into files in the\r\n'/AutoGenerated Grazes/_Predictions/'\r\ndirectory");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(180, 27));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(66, 13));
			((Control)label19).set_TabIndex(107);
			((Control)label19).set_Text("For grazes");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(31, 27));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(98, 13));
			((Control)label14).set_TabIndex(106);
			((Control)label14).set_Text("For occultations");
			((Control)chkZip).set_AutoSize(true);
			chkZip.set_Checked(Settings.Default.LunarZipFiles);
			chkZip.set_CheckState((CheckState)1);
			((Control)chkZip).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarZipFiles", true, (DataSourceUpdateMode)1));
			((Control)chkZip).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkZip).set_Location(new Point(14, 46));
			((Control)chkZip).set_Name("chkZip");
			((Control)chkZip).set_Size(new Size(131, 17));
			((Control)chkZip).set_TabIndex(87);
			((Control)chkZip).set_Text("Zip the generated files");
			((ButtonBase)chkZip).set_UseVisualStyleBackColor(true);
			chkZip.add_CheckedChanged((EventHandler)chkZip_CheckedChanged);
			((Control)chkDelete).set_AutoSize(true);
			chkDelete.set_Checked(Settings.Default.LunarDeleteZippedFiles);
			((Control)chkDelete).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarDeleteZippedFiles", true, (DataSourceUpdateMode)1));
			((Control)chkDelete).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkDelete).set_Location(new Point(14, 67));
			((Control)chkDelete).set_Name("chkDelete");
			((Control)chkDelete).set_Size(new Size(140, 17));
			((Control)chkDelete).set_TabIndex(88);
			((Control)chkDelete).set_Text("Delete files after Zipping");
			((ButtonBase)chkDelete).set_UseVisualStyleBackColor(true);
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(31, 160));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(91, 13));
			((Control)label13).set_TabIndex(7);
			((Control)label13).set_Text("for web sites...");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 9.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(14, 24));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(109, 16));
			((Control)label8).set_TabIndex(0);
			((Control)label8).set_Text("Set output files\r\n");
			label8.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(193, 208));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(175, 13));
			((Control)label50).set_TabIndex(23);
			((Control)label50).set_Text("Librations for plotting observed data");
			label50.set_TextAlign(ContentAlignment.MiddleRight);
			cmbLibrations.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLibrations).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLibrations).set_FormattingEnabled(true);
			cmbLibrations.get_Items().AddRange(new object[10] { "None", "P, D[+/- 0.3 deg]", "P, D[+/- 0.5 deg]", "P, D[+/- 1.0 deg]", "WA, L[+/- 1 deg], B[+/- 0.5 deg]", "WA, L[+/- 2 deg], B[+/- 0.5 deg]", "WA, L[+/- 1 deg], B[+/- 1 deg]", "WA, L[+/- 2 deg], B[+/- 1 deg]", "WA, L[+/- 4 deg], B[+/- 1 deg]", "WA, L[+/- 4 deg], B[+/- 2 deg]" });
			((Control)cmbLibrations).set_Location(new Point(196, 222));
			cmbLibrations.set_MaxDropDownItems(10);
			((Control)cmbLibrations).set_Name("cmbLibrations");
			((Control)cmbLibrations).set_Size(new Size(176, 21));
			((Control)cmbLibrations).set_TabIndex(24);
			cmbLibrations.add_SelectedIndexChanged((EventHandler)cmbLibrations_SelectedIndexChanged);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(181, 31));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(66, 13));
			((Control)label3).set_TabIndex(11);
			((Control)label3).set_Text("File format");
			cmbFileType.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbFileType).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbFileType).set_FormattingEnabled(true);
			cmbFileType.get_Items().AddRange(new object[5] { "JPEG", "BMP", "GIF", "PNG", "TIFF" });
			((Control)cmbFileType).set_Location(new Point(248, 27));
			((Control)cmbFileType).set_Name("cmbFileType");
			((Control)cmbFileType).set_Size(new Size(65, 21));
			((Control)cmbFileType).set_TabIndex(12);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(181, 46));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(29, 13));
			((Control)label2).set_TabIndex(13);
			((Control)label2).set_Text("Plot");
			((Control)groupBox1).get_Controls().Add((Control)(object)lblRange);
			((Control)groupBox1).get_Controls().Add((Control)(object)label1);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdNone);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdAll);
			((Control)groupBox1).get_Controls().Add((Control)(object)lstSites);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkFilter);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)groupBox1).get_Controls().Add((Control)(object)lblCount);
			((Control)groupBox1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox1).set_Location(new Point(5, 24));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(165, 552));
			((Control)groupBox1).set_TabIndex(95);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("1.  Select Sites");
			((Control)lblRange).set_AutoSize(true);
			((Control)lblRange).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblRange).set_Location(new Point(14, 40));
			((Control)lblRange).set_Name("lblRange");
			((Control)lblRange).set_Size(new Size(70, 12));
			((Control)lblRange).set_TabIndex(94);
			((Control)lblRange).set_Text("Long, lat ranges");
			((Control)chkFilter).set_AutoSize(true);
			chkFilter.set_Checked(Settings.Default.LunarElements_FilterBySite);
			chkFilter.set_CheckState((CheckState)1);
			((Control)chkFilter).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarElements_FilterBySite", true, (DataSourceUpdateMode)1));
			((Control)chkFilter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkFilter).set_Location(new Point(17, 57));
			((Control)chkFilter).set_Name("chkFilter");
			((Control)chkFilter).set_Size(new Size(116, 17));
			((Control)chkFilter).set_TabIndex(78);
			((Control)chkFilter).set_Text("Filter search by site");
			((ButtonBase)chkFilter).set_UseVisualStyleBackColor(true);
			((Control)lblCount).set_AutoSize(true);
			((Control)lblCount).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCount).set_Location(new Point(43, 471));
			((Control)lblCount).set_Name("lblCount");
			((Control)lblCount).set_Size(new Size(79, 13));
			((Control)lblCount).set_TabIndex(93);
			((Control)lblCount).set_Text("0 sites selected");
			((Control)groupBox2).get_Controls().Add((Control)(object)grpObjects);
			((Control)groupBox2).get_Controls().Add((Control)(object)grpXZ80);
			((Control)groupBox2).get_Controls().Add((Control)(object)labelIntegration);
			((Control)groupBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox2).set_Location(new Point(186, 24));
			((Control)groupBox2).set_Name("groupBox2");
			((Control)groupBox2).set_Size(new Size(375, 155));
			((Control)groupBox2).set_TabIndex(96);
			groupBox2.set_TabStop(false);
			((Control)groupBox2).set_Text("2.  Set Catalogue && Planets");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(63, 15));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(29, 13));
			((Control)label4).set_TabIndex(97);
			((Control)label4).set_Text("Year");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(109, 15));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(37, 13));
			((Control)label5).set_TabIndex(98);
			((Control)label5).set_Text("Month");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(154, 15));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(26, 13));
			((Control)label6).set_TabIndex(99);
			((Control)label6).set_Text("Day");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(9, 33));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(50, 13));
			((Control)label7).set_TabIndex(100);
			((Control)label7).set_Text("First date");
			((Control)optLarge).set_AutoSize(true);
			((Control)optLarge).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optLarge).set_Location(new Point(196, 88));
			((Control)optLarge).set_Name("optLarge");
			((Control)optLarge).set_Size(new Size(135, 17));
			((Control)optLarge).set_TabIndex(16);
			((Control)optLarge).set_Text("Large        [1024 x 768]");
			((ButtonBase)optLarge).set_UseVisualStyleBackColor(true);
			((Control)optLarge).add_Click((EventHandler)optLarge_Click);
			((Control)optMedium).set_AutoSize(true);
			optMedium.set_Checked(true);
			((Control)optMedium).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optMedium).set_Location(new Point(196, 73));
			((Control)optMedium).set_Name("optMedium");
			((Control)optMedium).set_Size(new Size(130, 17));
			((Control)optMedium).set_TabIndex(15);
			optMedium.set_TabStop(true);
			((Control)optMedium).set_Text("Medium     [800 x 600]");
			((ButtonBase)optMedium).set_UseVisualStyleBackColor(true);
			((Control)optMedium).add_Click((EventHandler)optMedium_Click);
			((Control)optSmall).set_AutoSize(true);
			((Control)optSmall).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSmall).set_Location(new Point(196, 58));
			((Control)optSmall).set_Name("optSmall");
			((Control)optSmall).set_Size(new Size(130, 17));
			((Control)optSmall).set_TabIndex(14);
			((Control)optSmall).set_Text("Small         [640 x 480]");
			((ButtonBase)optSmall).set_UseVisualStyleBackColor(true);
			((Control)optSmall).add_Click((EventHandler)optSmall_Click);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(89, 88));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(37, 13));
			((Control)label9).set_TabIndex(119);
			((Control)label9).set_Text("Month");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(135, 88));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(26, 13));
			((Control)label10).set_TabIndex(118);
			((Control)label10).set_Text("Day");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(172, 88));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(35, 13));
			((Control)label11).set_TabIndex(117);
			((Control)label11).set_Text("Today");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(11, 88));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(29, 13));
			((Control)label12).set_TabIndex(116);
			((Control)label12).set_Text("Year");
			((Control)button1).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)button1).set_Location(new Point(175, 100));
			((Control)button1).set_Name("button1");
			((Control)button1).set_Size(new Size(29, 17));
			((Control)button1).set_TabIndex(115);
			((ButtonBase)button1).set_UseVisualStyleBackColor(true);
			((Control)button1).add_Click((EventHandler)button1_Click);
			((Control)cmdFullMonth).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFullMonth).set_Location(new Point(93, 100));
			((Control)cmdFullMonth).set_Name("cmdFullMonth");
			((Control)cmdFullMonth).set_Size(new Size(29, 17));
			((Control)cmdFullMonth).set_TabIndex(113);
			((ButtonBase)cmdFullMonth).set_UseVisualStyleBackColor(true);
			((Control)cmdFullMonth).add_Click((EventHandler)cmdFullMonth_Click);
			((Control)cmdOneDay).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdOneDay).set_Location(new Point(134, 100));
			((Control)cmdOneDay).set_Name("cmdOneDay");
			((Control)cmdOneDay).set_Size(new Size(29, 17));
			((Control)cmdOneDay).set_TabIndex(114);
			((ButtonBase)cmdOneDay).set_UseVisualStyleBackColor(true);
			((Control)cmdOneDay).add_Click((EventHandler)cmdOneDay_Click);
			((Control)cmdFullYear).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFullYear).set_Location(new Point(11, 100));
			((Control)cmdFullYear).set_Name("cmdFullYear");
			((Control)cmdFullYear).set_Size(new Size(29, 17));
			((Control)cmdFullYear).set_TabIndex(112);
			((ButtonBase)cmdFullYear).set_UseVisualStyleBackColor(true);
			((Control)cmdFullYear).add_Click((EventHandler)cmdFullYear_Click);
			((Control)groupBox3).get_Controls().Add((Control)(object)label18);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmd3Mths);
			((Control)groupBox3).get_Controls().Add((Control)(object)label9);
			((Control)groupBox3).get_Controls().Add((Control)(object)label10);
			((Control)groupBox3).get_Controls().Add((Control)(object)label11);
			((Control)groupBox3).get_Controls().Add((Control)(object)label12);
			((Control)groupBox3).get_Controls().Add((Control)(object)button1);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdFullMonth);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdOneDay);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmdFullYear);
			((Control)groupBox3).get_Controls().Add((Control)(object)label7);
			((Control)groupBox3).get_Controls().Add((Control)(object)label6);
			((Control)groupBox3).get_Controls().Add((Control)(object)label5);
			((Control)groupBox3).get_Controls().Add((Control)(object)label4);
			((Control)groupBox3).get_Controls().Add((Control)(object)label28);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarDayEnd);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarDayStart);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarMonthEnd);
			((Control)groupBox3).get_Controls().Add((Control)(object)cmbLunarMonthStart);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLunarYearEnd);
			((Control)groupBox3).get_Controls().Add((Control)(object)updnLunarYearStart);
			((Control)groupBox3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox3).set_Location(new Point(577, 24));
			((Control)groupBox3).set_Name("groupBox3");
			((Control)groupBox3).set_Size(new Size(212, 129));
			((Control)groupBox3).set_TabIndex(120);
			groupBox3.set_TabStop(false);
			((Control)groupBox3).set_Text("5.  Set date range");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(47, 88));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(39, 13));
			((Control)label18).set_TabIndex(121);
			((Control)label18).set_Text("3-Mths");
			((Control)cmd3Mths).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmd3Mths).set_Location(new Point(52, 100));
			((Control)cmd3Mths).set_Name("cmd3Mths");
			((Control)cmd3Mths).set_Size(new Size(29, 17));
			((Control)cmd3Mths).set_TabIndex(120);
			((ButtonBase)cmd3Mths).set_UseVisualStyleBackColor(true);
			((Control)cmd3Mths).add_Click((EventHandler)cmd3Mths_Click);
			((Control)groupBox4).get_Controls().Add((Control)(object)panel2);
			((Control)groupBox4).get_Controls().Add((Control)(object)optMedium);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmbFileType);
			((Control)groupBox4).get_Controls().Add((Control)(object)panel1);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkNumberOfEvents);
			((Control)groupBox4).get_Controls().Add((Control)(object)label22);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkUsePoints);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkUseHires);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkUseLoRes);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkSpaces);
			((Control)groupBox4).get_Controls().Add((Control)(object)label17);
			((Control)groupBox4).get_Controls().Add((Control)(object)label16);
			((Control)groupBox4).get_Controls().Add((Control)(object)label15);
			((Control)groupBox4).get_Controls().Add((Control)(object)optLarge);
			((Control)groupBox4).get_Controls().Add((Control)(object)optSmall);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkRegional);
			((Control)groupBox4).get_Controls().Add((Control)(object)label13);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkIncludeLocalHTML);
			((Control)groupBox4).get_Controls().Add((Control)(object)label2);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkIncludeText);
			((Control)groupBox4).get_Controls().Add((Control)(object)label50);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkIncludeHTML);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkIncludeProfile);
			((Control)groupBox4).get_Controls().Add((Control)(object)cmbLibrations);
			((Control)groupBox4).get_Controls().Add((Control)(object)chkIncludeKML);
			((Control)groupBox4).get_Controls().Add((Control)(object)label8);
			((Control)groupBox4).get_Controls().Add((Control)(object)label3);
			((Control)groupBox4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)groupBox4).set_Location(new Point(186, 189));
			((Control)groupBox4).set_Name("groupBox4");
			((Control)groupBox4).set_Size(new Size(375, 270));
			((Control)groupBox4).set_TabIndex(121);
			groupBox4.set_TabStop(false);
			((Control)groupBox4).set_Text("3.  Options for grazes");
			((Control)panel2).get_Controls().Add((Control)(object)label24);
			((Control)panel2).get_Controls().Add((Control)(object)optBandW);
			((Control)panel2).get_Controls().Add((Control)(object)optColor);
			((Control)panel2).set_Location(new Point(190, 104));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(138, 19));
			((Control)panel2).set_TabIndex(27);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(60, 3));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(16, 13));
			((Control)label24).set_TabIndex(2);
			((Control)label24).set_Text("or");
			((Control)optBandW).set_AutoSize(true);
			optBandW.set_Checked(true);
			((Control)optBandW).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optBandW).set_Location(new Point(78, 1));
			((Control)optBandW).set_Name("optBandW");
			((Control)optBandW).set_Size(new Size(49, 17));
			((Control)optBandW).set_TabIndex(1);
			optBandW.set_TabStop(true);
			((Control)optBandW).set_Text("B&&W");
			((ButtonBase)optBandW).set_UseVisualStyleBackColor(true);
			((Control)optColor).set_AutoSize(true);
			optColor.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)optColor).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optColor).set_Location(new Point(9, 1));
			((Control)optColor).set_Name("optColor");
			((Control)optColor).set_Size(new Size(49, 17));
			((Control)optColor).set_TabIndex(0);
			((Control)optColor).set_Text("Color");
			((ButtonBase)optColor).set_UseVisualStyleBackColor(true);
			((Control)panel1).get_Controls().Add((Control)(object)optLOLA);
			((Control)panel1).set_Location(new Point(182, 137));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(147, 20));
			((Control)panel1).set_TabIndex(26);
			((Control)optLOLA).set_AutoSize(true);
			((Control)optLOLA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optLOLA).set_Location(new Point(6, 1));
			((Control)optLOLA).set_Name("optLOLA");
			((Control)optLOLA).set_Size(new Size(52, 17));
			((Control)optLOLA).set_TabIndex(0);
			((Control)optLOLA).set_Text("LOLA");
			((ButtonBase)optLOLA).set_UseVisualStyleBackColor(true);
			((Control)chkNumberOfEvents).set_AutoSize(true);
			chkNumberOfEvents.set_Checked(Settings.Default.AutoPredictNumberOfEvents);
			((Control)chkNumberOfEvents).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoPredictNumberOfEvents", true, (DataSourceUpdateMode)1));
			((Control)chkNumberOfEvents).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkNumberOfEvents).set_Location(new Point(187, 174));
			((Control)chkNumberOfEvents).set_Name("chkNumberOfEvents");
			((Control)chkNumberOfEvents).set_Size(new Size(153, 17));
			((Control)chkNumberOfEvents).set_TabIndex(21);
			((Control)chkNumberOfEvents).set_Text("Number of events indicator");
			((ButtonBase)chkNumberOfEvents).set_UseVisualStyleBackColor(true);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 7.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(181, 123));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(106, 13));
			((Control)label22).set_TabIndex(17);
			((Control)label22).set_Text("LOLA profile data");
			((Control)chkUsePoints).set_AutoSize(true);
			chkUsePoints.set_Checked(Settings.Default.AutoPredictLOLAoints);
			((Control)chkUsePoints).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUsePoints).set_Location(new Point(315, 158));
			((Control)chkUsePoints).set_Name("chkUsePoints");
			((Control)chkUsePoints).set_Size(new Size(55, 17));
			((Control)chkUsePoints).set_TabIndex(20);
			((Control)chkUsePoints).set_Text("Points");
			((ButtonBase)chkUsePoints).set_UseVisualStyleBackColor(true);
			((Control)chkUseHires).set_AutoSize(true);
			chkUseHires.set_Checked(Settings.Default.AutoPredictLOLAHiRes);
			((Control)chkUseHires).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUseHires).set_Location(new Point(252, 158));
			((Control)chkUseHires).set_Name("chkUseHires");
			((Control)chkUseHires).set_Size(new Size(55, 17));
			((Control)chkUseHires).set_TabIndex(19);
			((Control)chkUseHires).set_Text("HiRes");
			((ButtonBase)chkUseHires).set_UseVisualStyleBackColor(true);
			((Control)chkUseLoRes).set_AutoSize(true);
			chkUseLoRes.set_Checked(Settings.Default.AutoPredictLOLOALoRes);
			((Control)chkUseLoRes).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUseLoRes).set_Location(new Point(187, 158));
			((Control)chkUseLoRes).set_Name("chkUseLoRes");
			((Control)chkUseLoRes).set_Size(new Size(57, 17));
			((Control)chkUseLoRes).set_TabIndex(18);
			((Control)chkUseLoRes).set_Text("LoRes");
			((ButtonBase)chkUseLoRes).set_UseVisualStyleBackColor(true);
			((Control)chkSpaces).set_AutoSize(true);
			chkSpaces.set_Checked(Settings.Default.LunarNoSpacesInFileName);
			((Control)chkSpaces).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarNoSpacesInFileName", true, (DataSourceUpdateMode)1));
			((Control)chkSpaces).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSpaces).set_Location(new Point(17, 191));
			((Control)chkSpaces).set_Name("chkSpaces");
			((Control)chkSpaces).set_Size(new Size(135, 17));
			((Control)chkSpaces).set_TabIndex(9);
			((Control)chkSpaces).set_Text("No spaces in filenames");
			((ButtonBase)chkSpaces).set_UseVisualStyleBackColor(true);
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(181, 195));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(90, 13));
			((Control)label17).set_TabIndex(22);
			((Control)label17).set_Text("Observed data");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 9.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(196, 9));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(107, 16));
			((Control)label16).set_TabIndex(10);
			((Control)label16).set_Text("Profile options");
			label16.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(34, 50));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(98, 13));
			((Control)label15).set_TabIndex(1);
			((Control)label15).set_Text("for individuals...");
			((Control)chkRegional).set_AutoSize(true);
			chkRegional.set_Checked(Settings.Default.LunarRegionalGrazeMap);
			chkRegional.set_CheckState((CheckState)1);
			((Control)chkRegional).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarRegionalGrazeMap", true, (DataSourceUpdateMode)1));
			((Control)chkRegional).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkRegional).set_Location(new Point(17, 100));
			((Control)chkRegional).set_Name("chkRegional");
			((Control)chkRegional).set_Size(new Size(91, 17));
			((Control)chkRegional).set_TabIndex(4);
			((Control)chkRegional).set_Text("Regional map");
			((ButtonBase)chkRegional).set_UseVisualStyleBackColor(true);
			((Control)chkIncludeLocalHTML).set_AutoSize(true);
			chkIncludeLocalHTML.set_Checked(Settings.Default.LunarIncludeLocalHTML);
			((Control)chkIncludeLocalHTML).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarIncludeLocalHTML", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeLocalHTML).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIncludeLocalHTML).set_Location(new Point(17, 134));
			((Control)chkIncludeLocalHTML).set_Name("chkIncludeLocalHTML");
			((Control)chkIncludeLocalHTML).set_Size(new Size(137, 17));
			((Control)chkIncludeLocalHTML).set_TabIndex(6);
			((Control)chkIncludeLocalHTML).set_Text("Local GoogleMap HTM");
			((ButtonBase)chkIncludeLocalHTML).set_UseVisualStyleBackColor(true);
			((Control)chkIncludeText).set_AutoSize(true);
			chkIncludeText.set_Checked(Settings.Default.LunarIncludeGrazeText);
			chkIncludeText.set_CheckState((CheckState)1);
			((Control)chkIncludeText).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarIncludeGrazeText", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeText).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIncludeText).set_Location(new Point(17, 66));
			((Control)chkIncludeText).set_Name("chkIncludeText");
			((Control)chkIncludeText).set_Size(new Size(99, 17));
			((Control)chkIncludeText).set_TabIndex(2);
			((Control)chkIncludeText).set_Text("Prediction - text");
			((ButtonBase)chkIncludeText).set_UseVisualStyleBackColor(true);
			((Control)chkIncludeHTML).set_AutoSize(true);
			chkIncludeHTML.set_Checked(Settings.Default.LunarIncludeHTML);
			((Control)chkIncludeHTML).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarIncludeHTML", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeHTML).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIncludeHTML).set_Location(new Point(17, 174));
			((Control)chkIncludeHTML).set_Name("chkIncludeHTML");
			((Control)chkIncludeHTML).set_Size(new Size(111, 17));
			((Control)chkIncludeHTML).set_TabIndex(8);
			((Control)chkIncludeHTML).set_Text("GoogleMap  HTM");
			((ButtonBase)chkIncludeHTML).set_UseVisualStyleBackColor(true);
			((Control)chkIncludeProfile).set_AutoSize(true);
			chkIncludeProfile.set_Checked(Settings.Default.LunarIncludeProfile);
			chkIncludeProfile.set_CheckState((CheckState)1);
			((Control)chkIncludeProfile).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarIncludeProfile", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeProfile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIncludeProfile).set_Location(new Point(17, 83));
			((Control)chkIncludeProfile).set_Name("chkIncludeProfile");
			((Control)chkIncludeProfile).set_Size(new Size(55, 17));
			((Control)chkIncludeProfile).set_TabIndex(3);
			((Control)chkIncludeProfile).set_Text("Profile");
			((ButtonBase)chkIncludeProfile).set_UseVisualStyleBackColor(true);
			((Control)chkIncludeKML).set_AutoSize(true);
			chkIncludeKML.set_Checked(Settings.Default.LunarIncludeGoogleEarthKML);
			((Control)chkIncludeKML).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarIncludeGoogleEarthKML", true, (DataSourceUpdateMode)1));
			((Control)chkIncludeKML).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkIncludeKML).set_Location(new Point(17, 117));
			((Control)chkIncludeKML).set_Name("chkIncludeKML");
			((Control)chkIncludeKML).set_Size(new Size(111, 17));
			((Control)chkIncludeKML).set_TabIndex(5);
			((Control)chkIncludeKML).set_Text("GoogleEarth KMZ");
			((ButtonBase)chkIncludeKML).set_UseVisualStyleBackColor(true);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmbMonths);
			((Control)groupBox5).get_Controls().Add((Control)(object)cmdComputeFixed);
			((Control)groupBox5).get_Controls().Add((Control)(object)label21);
			((Control)groupBox5).get_Controls().Add((Control)(object)updnFixedYear);
			((Control)groupBox5).set_Location(new Point(579, 410));
			((Control)groupBox5).set_Name("groupBox5");
			((Control)groupBox5).set_Size(new Size(312, 152));
			((Control)groupBox5).set_TabIndex(122);
			groupBox5.set_TabStop(false);
			((Control)groupBox5).set_Text("6.  Fixed region bright grazes");
			cmbMonths.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbMonths).set_FormattingEnabled(true);
			cmbMonths.get_Items().AddRange(new object[7] { "Whole year", "January - June", "July - December", "January - March", "April - June", "July - September", "October - December" });
			((Control)cmbMonths).set_Location(new Point(161, 28));
			((Control)cmbMonths).set_Name("cmbMonths");
			((Control)cmbMonths).set_Size(new Size(129, 21));
			((Control)cmbMonths).set_TabIndex(100);
			((Control)cmdComputeFixed).set_Location(new Point(107, 81));
			((Control)cmdComputeFixed).set_Name("cmdComputeFixed");
			((Control)cmdComputeFixed).set_Size(new Size(99, 49));
			((Control)cmdComputeFixed).set_TabIndex(99);
			((Control)cmdComputeFixed).set_Text("Compute Fixed-region grazes");
			((ButtonBase)cmdComputeFixed).set_UseVisualStyleBackColor(true);
			((Control)cmdComputeFixed).add_Click((EventHandler)cmdComputeFixed_Click);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(22, 32));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(86, 13));
			((Control)label21).set_TabIndex(98);
			((Control)label21).set_Text("Prediction period");
			((Control)updnFixedYear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnFixedYear).set_Location(new Point(110, 29));
			updnFixedYear.set_Maximum(new decimal(new int[4] { 9999, 0, 0, 0 }));
			updnFixedYear.set_Minimum(new decimal(new int[4] { 5000, 0, 0, -2147483648 }));
			((Control)updnFixedYear).set_Name("updnFixedYear");
			((Control)updnFixedYear).set_Size(new Size(51, 20));
			((Control)updnFixedYear).set_TabIndex(67);
			updnFixedYear.set_Value(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(903, 24));
			((Control)menuStrip1).set_TabIndex(123);
			((Control)menuStrip1).set_Text("menuStrip1");
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
			((Control)chkShortOutput).set_AutoSize(true);
			chkShortOutput.set_Checked(Settings.Default.MultipredictShortOutputLine);
			((Control)chkShortOutput).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "MultipredictShortOutputLine", true, (DataSourceUpdateMode)1));
			((Control)chkShortOutput).set_Location(new Point(4, 76));
			((Control)chkShortOutput).set_Name("chkShortOutput");
			((Control)chkShortOutput).set_Size(new Size(146, 30));
			((Control)chkShortOutput).set_TabIndex(124);
			((Control)chkShortOutput).set_Text("Use short output line\r\n[essential for BAA/RASC]");
			((ButtonBase)chkShortOutput).set_UseVisualStyleBackColor(true);
			chkShortOutput.add_CheckedChanged((EventHandler)chkShortOutput_CheckedChanged);
			cmbLimitingMagnitude.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbLimitingMagnitude).set_FormattingEnabled(true);
			cmbLimitingMagnitude.get_Items().AddRange(new object[21]
			{
				"0.0", "1.0", "2.0", "3.0", "4.0", "4.5", "5.0", "5.5", "6.0", "6.5",
				"7.0", "7.5", "8.0", "8.5", "9.0", "9.5", "10.0", "10.5", "11.0", "11.5",
				"12.0"
			});
			((Control)cmbLimitingMagnitude).set_Location(new Point(19, 135));
			cmbLimitingMagnitude.set_MaxDropDownItems(14);
			((Control)cmbLimitingMagnitude).set_Name("cmbLimitingMagnitude");
			((Control)cmbLimitingMagnitude).set_Size(new Size(50, 21));
			((Control)cmbLimitingMagnitude).set_TabIndex(125);
			cmbLimitingMagnitude.add_SelectedIndexChanged((EventHandler)cmbLimitingMagnitude_SelectedIndexChanged);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(74, 132));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(56, 26));
			((Control)label23).set_TabIndex(126);
			((Control)label23).set_Text("Limiting\r\nmagnitude\r\n");
			((Control)groupBox6).get_Controls().Add((Control)(object)chkUseCA);
			((Control)groupBox6).get_Controls().Add((Control)(object)label23);
			((Control)groupBox6).get_Controls().Add((Control)(object)chkShortOutput);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmbLimitingMagnitude);
			((Control)groupBox6).get_Controls().Add((Control)(object)cmdOccultationsForAll);
			((Control)groupBox6).set_Location(new Point(733, 215));
			((Control)groupBox6).set_Name("groupBox6");
			((Control)groupBox6).set_Size(new Size(143, 192));
			((Control)groupBox6).set_TabIndex(127);
			groupBox6.set_TabStop(false);
			((Control)chkUseCA).set_AutoSize(true);
			chkUseCA.set_Checked(Settings.Default.MultipredictUseCA);
			((Control)chkUseCA).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "MultipredictUseCA", true, (DataSourceUpdateMode)1));
			((Control)chkUseCA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUseCA).set_Location(new Point(4, 108));
			((Control)chkUseCA).set_Name("chkUseCA");
			((Control)chkUseCA).set_Size(new Size(137, 17));
			((Control)chkUseCA).set_TabIndex(127);
			((Control)chkUseCA).set_Text("CA-%ILL [not PA-Elong]");
			((ButtonBase)chkUseCA).set_UseVisualStyleBackColor(true);
			((Control)cmdMergeBAA).set_Location(new Point(749, 378));
			((Control)cmdMergeBAA).set_Name("cmdMergeBAA");
			((Control)cmdMergeBAA).set_Size(new Size(113, 22));
			((Control)cmdMergeBAA).set_TabIndex(127);
			((Control)cmdMergeBAA).set_Text("Merge [BAA/RASC]");
			((ButtonBase)cmdMergeBAA).set_UseVisualStyleBackColor(true);
			((Control)cmdMergeBAA).add_Click((EventHandler)cmdMergeBAA_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(903, 588));
			((Control)this).get_Controls().Add((Control)(object)cmdMergeBAA);
			((Control)this).get_Controls().Add((Control)(object)groupBox6);
			((Control)this).get_Controls().Add((Control)(object)groupBox5);
			((Control)this).get_Controls().Add((Control)(object)groupBox4);
			((Control)this).get_Controls().Add((Control)(object)groupBox3);
			((Control)this).get_Controls().Add((Control)(object)groupBox2);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)grpOutputs);
			((Control)this).get_Controls().Add((Control)(object)lblCurrentDate);
			((Control)this).get_Controls().Add((Control)(object)pBar2);
			((Control)this).get_Controls().Add((Control)(object)pBar);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdGrazes);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarAutoGenerat", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_FormBorderStyle((FormBorderStyle)2);
			((Form)this).set_Location(Settings.Default.LocationLunarAutoGenerat);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("LunarAutoGenerate");
			((Control)this).set_Text("Predictions for multiple objects at multiple sites");
			((Form)this).add_FormClosed(new FormClosedEventHandler(LunarAutoGenerate_FormClosed));
			((Form)this).add_Load((EventHandler)LunarAutoGenerate_Load);
			((Control)grpObjects).ResumeLayout(false);
			((Control)grpObjects).PerformLayout();
			((Control)grpXZ80).ResumeLayout(false);
			((Control)grpXZ80).PerformLayout();
			((ISupportInitialize)updnLunarYearEnd).EndInit();
			((ISupportInitialize)updnLunarYearStart).EndInit();
			((Control)grpOutputs).ResumeLayout(false);
			((Control)grpOutputs).PerformLayout();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)groupBox2).ResumeLayout(false);
			((Control)groupBox2).PerformLayout();
			((Control)groupBox3).ResumeLayout(false);
			((Control)groupBox3).PerformLayout();
			((Control)groupBox4).ResumeLayout(false);
			((Control)groupBox4).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)groupBox5).ResumeLayout(false);
			((Control)groupBox5).PerformLayout();
			((ISupportInitialize)updnFixedYear).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)groupBox6).ResumeLayout(false);
			((Control)groupBox6).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
