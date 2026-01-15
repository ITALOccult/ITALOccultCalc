using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Asteroids;
using Occult.Ephemerides;
using Occult.Properties;

namespace Occult
{
	public class UserAsteroids_Edit : Form
	{
		private readonly string AppPath;

		private readonly string ElementFile;

		private readonly string MainElementFile;

		public int FirstRecord;

		public int LastRecord;

		private const int ElementFileLength = 143;

		private ArrayList CometElements;

		private bool IsLunar;

		private bool UnsavedEdits;

		private bool Abort;

		private int xPos_Shape;

		private int yPos_Shape;

		private int CheckCount;

		private CometLastObs CometLast;

		private List<CometLastObs> CometList = new List<CometLastObs>();

		private IContainer components;

		private ListBox lstElements;

		private Label label1;

		private Label label2;

		private Button cmdDeleteAll;

		private Button cmdDeleteSelected;

		private Button cmdAddFromFile;

		private Button cmdAddH0Limited;

		private GroupBox groupOppMag;

		private NumericUpDown updnMag;

		private Label label3;

		private Button cmdSave;

		private TextBox txtNumber;

		private TextBox txtName;

		private TextBox txtMA;

		private NumericUpDown updnYear;

		private NumericUpDown updnMonth;

		private NumericUpDown updnDay;

		private TextBox txtA;

		private TextBox txte;

		private TextBox txtI;

		private TextBox txtNode;

		private TextBox txtPerihelion;

		private TextBox txtError;

		private TextBox txtMaxD;

		private TextBox txtG;

		private TextBox txtH0;

		private Label label4;

		private Label label5;

		private Label label7;

		private Label label8;

		private Label label9;

		private Label label10;

		private Label label11;

		private Label label12;

		private Label label13;

		private Label label14;

		private Label label15;

		private Label label16;

		private Label label17;

		private GroupBox grpEdit;

		private Button cmdAddElements;

		private Button cmdReplaceSelected;

		private Label label18;

		private ComboBox cmbSource;

		private Button cmdCancel;

		private Button cmdEdit;

		private Panel panelMain;

		private Label label19;

		private Label label20;

		private ComboBox cmbMoons;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private Button cmdImportComet;

		private ComboBox cmbComet;

		private Label label21;

		private GroupBox grpComets;

		private GroupBox groupBox1;

		private ToolStripMenuItem pasteElementsFromJPLHorizonsToolStripMenuItem;

		private Button cmdImportOELfile;

		private Button cmdImportAllCurrentComets;

		private Label label22;

		private RadioButton optAll;

		private RadioButton opt18;

		private RadioButton opt14;

		private Panel panel1;

		private Label label23;

		private ComboBox cmbRings;

		private Panel panelData;

		private Label label24;

		private TextBox txtOrbitSrc;

		private Label label25;

		private TextBox txtOrbitDate;

		private ToolStripMenuItem fileToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ToolStripMenuItem reloadFromFileToolStripMenuItem;

		private ComboBox cmbAsteroidClasses;

		private Label label6;

		private Panel pnlHorizons;

		private Button cmdHorizons;

		private Label label31;

		private TextBox txtAsteroids;

		private Label label30;

		private Label label29;

		private Label label28;

		private Label label27;

		private Label label26;

		private TextBox txtMonth;

		private TextBox txtDay;

		private TextBox txtYear;

		private Button cmdExit;

		private Button cmdAbort;

		private Label label32;

		private Label lblCurrentNum;

		private Label lbl400;

		private Label lbl200;

		private Label lbl100;

		private Label lbl50;

		private RadioButton opt100;

		private RadioButton opt200;

		private RadioButton opt400;

		private RadioButton optNumber;

		private RadioButton opt50;

		private Button cmdHelp;

		private Label lbl20;

		private RadioButton opt20;

		private Label lbl30;

		private RadioButton opt30;

		private Label lbl40;

		private RadioButton opt40;

		private Label label33;

		private Label label34;

		private TextBox txtHour;

		private Panel panel2;

		private Label label36;

		private Label label35;

		private TextBox txtMinute;

		private Panel panelAsteroid;

		private Panel panel3;

		private ComboBox cmbCometHorizons;

		private Label label37;

		private Button cmdCometFromHorizons;

		private CheckedListBox chkLstComets;

		private Button cmdClearElements;

		private Button cmdClearChecks;

		private Button cmdUpdateComets;

		private Label lbl15;

		private RadioButton opt15;

		private Label lbl10;

		private RadioButton opt10;

		private Label lbl5;

		private RadioButton opt5;

		private Label label38;

		public UserAsteroids_Edit(bool LunarOccultations)
		{
			InitializeComponent();
			IsLunar = LunarOccultations;
			AppPath = Utilities.AppPath;
			((ListControl)cmbRings).set_SelectedIndex(0);
			((ListControl)cmbMoons).set_SelectedIndex(0);
			((ListControl)cmbAsteroidClasses).set_SelectedIndex(0);
			((ListControl)cmbSource).set_SelectedIndex(0);
			if (LunarOccultations)
			{
				ElementFile = AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv";
				((Control)this).set_Text("Edit User file of minor planets, for Lunar occultations");
			}
			else
			{
				ElementFile = AppPath + "\\Resource Files\\UserMinorPlanetElements.csv";
				((Control)this).set_Text("Edit User file of minor planets, for occultations by those minor planets");
				((Control)groupOppMag).set_Enabled(false);
			}
			MainElementFile = AppPath + "\\Resource Files\\AsteroidElements.csv";
			if (!File.Exists(MainElementFile))
			{
				((Control)cmdAddFromFile).set_Enabled(false);
				((Control)cmdAddH0Limited).set_Enabled(false);
			}
		}

		private void UserAsteroids_Edit_Load(object sender, EventArgs e)
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
			((Control)label1).set_Text("                          epoch [or T]      M.A.       perihelion     node      inclination    e              a [or q]     Magnitudes  diam  err src Err  # # Orbit     Orbit     Taxonomic");
			((Control)label2).set_Text(" number Name                y m  d           o            o            o            o                                       H0    G     km   ±km     \"    ⊚ ☾ src       date      class");
			Elements.UserAsteroids.SourceFile = ElementFile;
			if (IsLunar)
			{
				Elements.UserAsteroids_Lunar.Fill_AllAsteroids();
			}
			else
			{
				Elements.UserAsteroids.Fill_AllAsteroids();
			}
			DisplayElements();
			if (lstElements.get_Items().get_Count() > 0)
			{
				((ListControl)lstElements).set_SelectedIndex(0);
			}
			Button obj = cmdImportComet;
			bool enabled;
			((Control)cmbComet).set_Enabled(enabled = File.Exists(AppPath + "\\Downloaded Files\\Comet.dat"));
			((Control)obj).set_Enabled(enabled);
			if (!((Control)cmdImportComet).get_Enabled())
			{
				return;
			}
			CometElements = new ArrayList();
			cmbComet.get_Items().Clear();
			using (StreamReader streamReader = new StreamReader(AppPath + "\\Downloaded Files\\Comet.dat"))
			{
				do
				{
					string text = streamReader.ReadLine()!.PadRight(165);
					CometElements.Add(text);
					cmbComet.get_Items().Add((object)text.Substring(102, 57).Trim());
					cmbCometHorizons.get_Items().Add((object)text.Substring(102, 57).Trim());
					((ObjectCollection)chkLstComets.get_Items()).Add((object)text.Substring(102, 57).Trim());
				}
				while (!streamReader.EndOfStream);
			}
			if (cmbComet.get_Items().get_Count() >= 0)
			{
				CheckedListBox obj2 = chkLstComets;
				ComboBox obj3 = cmbCometHorizons;
				int num;
				((ListControl)cmbComet).set_SelectedIndex(num = cmbComet.get_Items().get_Count() - 1);
				int selectedIndex;
				((ListControl)obj3).set_SelectedIndex(selectedIndex = num);
				((ListControl)obj2).set_SelectedIndex(selectedIndex);
			}
			((Control)cmdCometFromHorizons).set_Text(string.Format("Retrieve {0,1} elements", CheckCount));
		}

		private void DisplayElements()
		{
			lstElements.get_Items().Clear();
			if (IsLunar)
			{
				if (Elements.UserAsteroids_Lunar.AstElements.Count >= 1)
				{
					Elements.UserAsteroids_Lunar.AstElements.Sort();
					for (int i = 0; i < Elements.UserAsteroids_Lunar.AstElements.Count; i++)
					{
						lstElements.get_Items().Add((object)Elements.UserAsteroids_Lunar.AstElements[i].UserEditString());
					}
				}
			}
			else if (Elements.UserAsteroids.AstElements.Count >= 1)
			{
				Elements.UserAsteroids.AstElements.Sort();
				for (int j = 0; j < Elements.UserAsteroids.AstElements.Count; j++)
				{
					lstElements.get_Items().Add((object)Elements.UserAsteroids.AstElements[j].UserEditString());
				}
			}
		}

		private void cmdDeleteAll_Click(object sender, EventArgs e)
		{
			DeleteAll();
		}

		private void DeleteAll()
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Do you want to delete ALL entries?", "Delete entries", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				if (IsLunar)
				{
					Elements.UserAsteroids_Lunar.AstElements.Clear();
				}
				else
				{
					Elements.UserAsteroids.AstElements.Clear();
				}
				DisplayElements();
				UnsavedEdits = true;
			}
		}

		private void cmdDeleteSelected_Click(object sender, EventArgs e)
		{
			//IL_0050: Unknown result type (might be due to invalid IL or missing references)
			//IL_0056: Invalid comparison between Unknown and I4
			int selectedIndex = ((ListControl)lstElements).get_SelectedIndex();
			if (selectedIndex >= 0 && (int)MessageBox.Show("Do you want to delete " + lstElements.get_Items().get_Item(selectedIndex).ToString()!.Substring(0, 23).Trim(), "Delete entries", (MessageBoxButtons)1, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 2)
			{
				lstElements.get_Items().RemoveAt(selectedIndex);
				if (IsLunar)
				{
					Elements.UserAsteroids_Lunar.AstElements.RemoveAt(selectedIndex);
				}
				else
				{
					Elements.UserAsteroids.AstElements.RemoveAt(selectedIndex);
				}
				DisplayElements();
				UnsavedEdits = true;
			}
		}

		private void cmdAddFromFile_Click(object sender, EventArgs e)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Invalid comparison between Unknown and I4
			AsteroidSelectForSearch asteroidSelectForSearch = new AsteroidSelectForSearch(UseUserElements: false, "*");
			if ((int)((Form)asteroidSelectForSearch).ShowDialog() == 1)
			{
				if (asteroidSelectForSearch.FirstRecord >= 0)
				{
					AddFromMainAsteroids(asteroidSelectForSearch.FirstRecord, asteroidSelectForSearch.LastRecord, ApplyMagLimit: false, 0.0);
				}
				UnsavedEdits = true;
			}
		}

		private void cmdAddH0Limited_Click(object sender, EventArgs e)
		{
			AddFromMainAsteroids(0, 2000, ApplyMagLimit: true, (double)updnMag.get_Value());
			UnsavedEdits = true;
		}

		private void AddFromMainAsteroids(int First, int Last, bool ApplyMagLimit, double magLimit)
		{
			if (Elements.MainAsteroids.AstElements.Count < 1)
			{
				Elements.MainAsteroids.Fill_AllAsteroids();
			}
			for (int i = First; i <= Last; i++)
			{
				if (!ApplyMagLimit)
				{
					if (IsLunar)
					{
						AsteroidElements asteroidElements = new AsteroidElements();
						asteroidElements = Elements.MainAsteroids.AstElements[i];
						Elements.UserAsteroids_Lunar.AstElements.Add(asteroidElements);
					}
					else
					{
						AsteroidElements asteroidElements2 = new AsteroidElements();
						asteroidElements2 = Elements.MainAsteroids.AstElements[i];
						Elements.UserAsteroids.AstElements.Add(asteroidElements2);
					}
					continue;
				}
				double a = Elements.MainAsteroids.AstElements[i].A;
				if (Elements.MainAsteroids.AstElements[i].H0 + 5.0 * Math.Log10(a * (a - 1.0)) <= magLimit)
				{
					if (IsLunar)
					{
						AsteroidElements asteroidElements3 = new AsteroidElements();
						asteroidElements3 = Elements.MainAsteroids.AstElements[i];
						Elements.UserAsteroids_Lunar.AstElements.Add(asteroidElements3);
					}
					else
					{
						AsteroidElements asteroidElements4 = new AsteroidElements();
						asteroidElements4 = Elements.MainAsteroids.AstElements[i];
						Elements.UserAsteroids.AstElements.Add(asteroidElements4);
					}
				}
			}
			DisplayElements();
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Save();
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			Save();
		}

		private void Save()
		{
			if (lstElements.get_Items().get_Count() < 1)
			{
				return;
			}
			if (IsLunar)
			{
				using StreamWriter streamWriter = new StreamWriter(Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements_Lunar.csv");
				streamWriter.WriteLine(AsteroidElements.CSVHeader());
				for (int i = 0; i < Elements.UserAsteroids_Lunar.AstElements.Count; i++)
				{
					streamWriter.WriteLine(Elements.UserAsteroids_Lunar.AstElements[i].CSVString());
				}
			}
			else
			{
				using StreamWriter streamWriter2 = new StreamWriter(Utilities.AppPath + "\\Resource Files\\UserMinorPlanetElements.csv");
				streamWriter2.WriteLine(AsteroidElements.CSVHeader());
				for (int j = 0; j < Elements.UserAsteroids.AstElements.Count; j++)
				{
					streamWriter2.WriteLine(Elements.UserAsteroids.AstElements[j].CSVString());
				}
			}
			UnsavedEdits = false;
		}

		private void lstElements_SelectedIndexChanged(object sender, EventArgs e)
		{
			int selectedIndex = ((ListControl)lstElements).get_SelectedIndex();
			if (selectedIndex < 0 || ((Control)pnlHorizons).get_Visible())
			{
				return;
			}
			if (IsLunar)
			{
				((Control)txtNumber).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].IDNumber.ToString());
				((Control)txtName).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].IDName.Trim());
				updnYear.set_Value((decimal)Elements.UserAsteroids_Lunar.AstElements[selectedIndex].EpochYear);
				updnMonth.set_Value((decimal)Elements.UserAsteroids_Lunar.AstElements[selectedIndex].EpochMonth);
				updnDay.set_Value((decimal)Elements.UserAsteroids_Lunar.AstElements[selectedIndex].EpochDay);
				((Control)txtMA).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].Meananomaly.ToString());
				((Control)txtPerihelion).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].Perihelion.ToString());
				((Control)txtNode).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].Node.ToString());
				((Control)txtI).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].I.ToString());
				((Control)txte).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].E.ToString());
				((Control)txtA).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].A.ToString());
				((Control)txtH0).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].H0.ToString());
				((Control)txtG).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].G_phaseCoeff.ToString());
				((Control)txtMaxD).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].Diameter_Mean.ToString());
				for (int i = 0; i < cmbAsteroidClasses.get_Items().get_Count(); i++)
				{
					if (!(cmbAsteroidClasses.get_Items().get_Item(i).ToString()!.ToUpper() == "PHA") && cmbAsteroidClasses.get_Items().get_Item(i).ToString()!.ToUpper().Contains(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].AsteroidClass.ToUpper()))
					{
						((ListControl)cmbAsteroidClasses).set_SelectedIndex(i);
						break;
					}
				}
				((ListControl)cmbSource).set_SelectedIndex("0123456789ABCDEFM".IndexOf(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].Dia_Source.Trim()));
				((Control)txtError).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].PeakEphemUncert.ToString());
				((ListControl)cmbRings).set_SelectedIndex(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].Num_Rings);
				((ListControl)cmbMoons).set_SelectedIndex(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].Num_Moons);
				((Control)txtOrbitSrc).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].OrbitSource);
				((Control)txtOrbitDate).set_Text(Elements.UserAsteroids_Lunar.AstElements[selectedIndex].OrbitDate);
				return;
			}
			((Control)txtNumber).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].IDNumber.ToString());
			((Control)txtName).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].IDName.Trim());
			updnYear.set_Value((decimal)Elements.UserAsteroids.AstElements[selectedIndex].EpochYear);
			updnMonth.set_Value((decimal)Elements.UserAsteroids.AstElements[selectedIndex].EpochMonth);
			updnDay.set_Value((decimal)Elements.UserAsteroids.AstElements[selectedIndex].EpochDay);
			((Control)txtMA).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].Meananomaly.ToString());
			((Control)txtPerihelion).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].Perihelion.ToString());
			((Control)txtNode).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].Node.ToString());
			((Control)txtI).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].I.ToString());
			((Control)txte).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].E.ToString());
			((Control)txtA).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].A.ToString());
			((Control)txtH0).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].H0.ToString());
			((Control)txtG).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].G_phaseCoeff.ToString());
			((Control)txtMaxD).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].Diameter_Mean.ToString());
			for (int j = 0; j < cmbAsteroidClasses.get_Items().get_Count(); j++)
			{
				if (!(cmbAsteroidClasses.get_Items().get_Item(j).ToString()!.ToUpper() == "PHA") && cmbAsteroidClasses.get_Items().get_Item(j).ToString()!.ToUpper().Contains(Elements.UserAsteroids.AstElements[selectedIndex].AsteroidClass.ToUpper()))
				{
					((ListControl)cmbAsteroidClasses).set_SelectedIndex(j);
					break;
				}
			}
			((ListControl)cmbSource).set_SelectedIndex("0123456789ABCDEFM".IndexOf(Elements.UserAsteroids.AstElements[selectedIndex].Dia_Source.Trim()));
			((Control)txtError).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].PeakEphemUncert.ToString());
			((ListControl)cmbRings).set_SelectedIndex(Elements.UserAsteroids.AstElements[selectedIndex].Num_Rings);
			((ListControl)cmbMoons).set_SelectedIndex(Elements.UserAsteroids.AstElements[selectedIndex].Num_Moons);
			((Control)txtOrbitSrc).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].OrbitSource);
			((Control)txtOrbitDate).set_Text(Elements.UserAsteroids.AstElements[selectedIndex].OrbitDate);
		}

		private void cmdEdit_Click(object sender, EventArgs e)
		{
			((Control)lstElements).set_Enabled(false);
			((Control)panelMain).set_Enabled(false);
			((Control)grpEdit).set_Enabled(true);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Control)lstElements).set_Enabled(true);
			((Control)panelMain).set_Enabled(true);
			((Control)grpEdit).set_Enabled(false);
		}

		private void cmdAddElements_Click(object sender, EventArgs e)
		{
			CreateLine(Replace: false);
		}

		private void cmdReplaceSelected_Click(object sender, EventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)lstElements).get_SelectedIndex() < 0)
			{
				MessageBox.Show("An element line has not been selected for editing. These elements can oinly be added.", "No selection", (MessageBoxButtons)0, (MessageBoxIcon)48);
			}
			else
			{
				CreateLine(Replace: true);
			}
		}

		private void CreateLine(bool Replace)
		{
			//IL_018a: Unknown result type (might be due to invalid IL or missing references)
			AsteroidElements asteroidElements = new AsteroidElements();
			int.TryParse(((Control)txtNumber).get_Text(), out var result);
			string text = ((Control)txtName).get_Text().Trim().PadRight(16);
			if (text.Length > 16)
			{
				text = text.Substring(0, 16);
			}
			int epochYear = (int)updnYear.get_Value();
			int epochMonth = (int)updnMonth.get_Value();
			double epochDay = (double)updnDay.get_Value();
			double result2;
			double result3;
			double result4;
			double result5;
			double result6;
			double result7;
			double result8;
			double result9;
			double result10;
			int num = 1 & (double.TryParse(((Control)txtMA).get_Text(), out result2) ? 1 : 0) & (double.TryParse(((Control)txtPerihelion).get_Text(), out result3) ? 1 : 0) & (double.TryParse(((Control)txtNode).get_Text(), out result4) ? 1 : 0) & (double.TryParse(((Control)txtI).get_Text(), out result5) ? 1 : 0) & (double.TryParse(((Control)txte).get_Text(), out result6) ? 1 : 0) & (double.TryParse(((Control)txtA).get_Text(), out result7) ? 1 : 0) & (double.TryParse(((Control)txtH0).get_Text(), out result8) ? 1 : 0) & (double.TryParse(((Control)txtG).get_Text(), out result9) ? 1 : 0) & (double.TryParse(((Control)txtMaxD).get_Text(), out result10) ? 1 : 0) & ((((ListControl)cmbAsteroidClasses).get_SelectedIndex() >= 0) ? 1 : 0) & ((((ListControl)cmbSource).get_SelectedIndex() >= 0) ? 1 : 0);
			string dia_Source = "0123456789ABCDEFM".Substring(((ListControl)cmbSource).get_SelectedIndex(), 1);
			if (((uint)num & (double.TryParse(((Control)txtError).get_Text(), out var result11) ? 1u : 0u)) == 0)
			{
				MessageBox.Show("One or more data fields are blank, or contain non-numeric data", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			asteroidElements.IDNumber = result;
			asteroidElements.IDName = text;
			asteroidElements.Meananomaly = result2;
			asteroidElements.EpochYear = epochYear;
			asteroidElements.EpochMonth = epochMonth;
			asteroidElements.EpochDay = epochDay;
			asteroidElements.Perihelion = result3;
			asteroidElements.Node = result4;
			asteroidElements.I = result5;
			asteroidElements.E = result6;
			asteroidElements.A = result7;
			asteroidElements.H0 = result8;
			asteroidElements.G_phaseCoeff = result9;
			asteroidElements.Diameter_Mean = result10;
			asteroidElements.Dia_Source = dia_Source;
			asteroidElements.PeakEphemUncert = result11;
			asteroidElements.Num_Rings = ((ListControl)cmbRings).get_SelectedIndex();
			asteroidElements.Num_Moons = ((ListControl)cmbMoons).get_SelectedIndex();
			asteroidElements.OrbitSource = ((Control)txtOrbitSrc).get_Text();
			asteroidElements.OrbitDate = ((Control)txtOrbitDate).get_Text();
			if (((ListControl)cmbAsteroidClasses).get_SelectedIndex() == 0)
			{
				asteroidElements.AsteroidClass = "";
			}
			else
			{
				asteroidElements.AsteroidClass = cmbAsteroidClasses.get_Items().get_Item(((ListControl)cmbAsteroidClasses).get_SelectedIndex()).ToString();
			}
			if (!Replace)
			{
				if (IsLunar)
				{
					Elements.UserAsteroids_Lunar.AstElements.Add(asteroidElements);
				}
				else
				{
					Elements.UserAsteroids.AstElements.Add(asteroidElements);
				}
			}
			else if (IsLunar)
			{
				Elements.UserAsteroids_Lunar.AstElements[((ListControl)lstElements).get_SelectedIndex()] = asteroidElements;
			}
			else
			{
				Elements.UserAsteroids.AstElements[((ListControl)lstElements).get_SelectedIndex()] = asteroidElements;
			}
			DisplayElements();
			((Control)lstElements).set_Enabled(true);
			((Control)panelMain).set_Enabled(true);
			((Control)grpEdit).set_Enabled(false);
			UnsavedEdits = true;
		}

		private void updnMag_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMag).Select(0, 10);
		}

		private void txtNumber_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtNumber).SelectAll();
		}

		private void txtName_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtName).SelectAll();
		}

		private void updnYear_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnYear).Select(0, 10);
		}

		private void updnMonth_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnMonth).Select(0, 10);
		}

		private void updnDay_Enter(object sender, EventArgs e)
		{
			((UpDownBase)updnDay).Select(0, 10);
		}

		private void txtMA_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMA).SelectAll();
		}

		private void txte_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txte).SelectAll();
		}

		private void txtA_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtA).SelectAll();
		}

		private void txtPerihelion_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtPerihelion).SelectAll();
		}

		private void txtNode_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtNode).SelectAll();
		}

		private void txtI_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtI).SelectAll();
		}

		private void txtH0_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtH0).SelectAll();
		}

		private void txtG_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtG).SelectAll();
		}

		private void txtError_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtError).SelectAll();
		}

		private void txtMaxD_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtMaxD).SelectAll();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"User asteroids Edit");
		}

		private void UserAsteroids_Edit_FormClosed(object sender, FormClosedEventArgs e)
		{
			((Component)this).Dispose();
		}

		private void cmdImportComet_Click(object sender, EventArgs e)
		{
			ImportElementsOfAComet(((ListControl)cmbComet).get_SelectedIndex());
			DisplayElements();
		}

		private void ImportElementsOfAComet(int EntryNum)
		{
			if (EntryNum < 0)
			{
				return;
			}
			string text = CometElements[EntryNum]!.ToString();
			double num = double.Parse(text.Substring(40, 9));
			AsteroidElements asteroidElements = new AsteroidElements();
			asteroidElements.E = num;
			asteroidElements.Perihelion = double.Parse(text.Substring(50, 9));
			asteroidElements.Node = double.Parse(text.Substring(60, 9));
			asteroidElements.I = double.Parse(text.Substring(70, 9));
			asteroidElements.H0 = double.Parse(text.Substring(90, 5));
			if ((num < 0.97) & (text.Substring(81, 4).Trim() != ""))
			{
				asteroidElements.EpochYear = int.Parse(text.Substring(81, 4));
				asteroidElements.EpochMonth = int.Parse(text.Substring(85, 2));
				asteroidElements.EpochDay = int.Parse(text.Substring(87, 2));
				double num2 = Utilities.JD_from_Date(int.Parse(text.Substring(14, 4)), int.Parse(text.Substring(19, 2)), double.Parse(text.Substring(22, 7)));
				asteroidElements.A = double.Parse(text.Substring(30, 9)) / (1.0 - num);
				double num3 = 0.01720209895 * Math.Pow(asteroidElements.A, -1.5);
				asteroidElements.Meananomaly = num3 * (Utilities.JD_from_Date(asteroidElements.EpochYear, asteroidElements.EpochMonth, asteroidElements.EpochDay) - num2) * (180.0 / Math.PI);
				if (asteroidElements.Meananomaly < 0.0)
				{
					asteroidElements.Meananomaly += 360.0;
				}
			}
			else
			{
				asteroidElements.EpochYear = int.Parse(text.Substring(14, 4));
				asteroidElements.EpochMonth = int.Parse(text.Substring(19, 2));
				asteroidElements.EpochDay = double.Parse(text.Substring(22, 7));
				asteroidElements.A = double.Parse(text.Substring(30, 9));
				asteroidElements.Meananomaly = 0.0;
			}
			asteroidElements.Dia_Source = "9";
			asteroidElements.Diameter_Mean = 10.0;
			asteroidElements.PeakEphemUncert = 3.0;
			asteroidElements.IDNumber = 0;
			asteroidElements.IDName = text.Substring(102, 57).Trim();
			int num4 = asteroidElements.IDName.IndexOf("P/");
			if (num4 > 0 && num4 < 4)
			{
				asteroidElements.IDName = "".PadRight(3 - num4) + asteroidElements.IDName;
			}
			if (num >= 0.97)
			{
				asteroidElements.OrbitSource = AsteroidElements.Sources[3] + ".NI";
			}
			else
			{
				asteroidElements.OrbitSource = AsteroidElements.Sources[3];
			}
			DateTime lastWriteTime = new FileInfo(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt").LastWriteTime;
			asteroidElements.OrbitDate = lastWriteTime.Year + Utilities.ShortMonths[lastWriteTime.Month] + lastWriteTime.Day.ToString().PadLeft(2, '0');
			if (IsLunar)
			{
				Elements.UserAsteroids_Lunar.AstElements.Add(asteroidElements);
			}
			else
			{
				Elements.UserAsteroids.AstElements.Add(asteroidElements);
			}
			UnsavedEdits = true;
		}

		private void PasteElementsFromJPLHorizonsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			int num = 0;
			int num2 = 0;
			int num3 = 0;
			int num4 = 0;
			int num5 = 0;
			int num6 = 0;
			int num7 = 0;
			int num8 = 0;
			int num9 = 0;
			int num10 = 0;
			((Control)pnlHorizons).set_Visible(true);
			((Control)txtYear).set_Text(DateTime.Now.ToUniversalTime().Year.ToString());
			((Control)txtMonth).set_Text(DateTime.Now.ToUniversalTime().Month.ToString());
			((Control)txtDay).set_Text(DateTime.Now.ToUniversalTime().Day.ToString());
			if (Elements.MainAsteroids.AstElements.Count < 1)
			{
				Elements.MainAsteroids.Fill_AllAsteroids();
			}
			for (int i = 0; i < Elements.MainAsteroids.AstElements.Count; i++)
			{
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 5.0)
				{
					num++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 10.0)
				{
					num2++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 15.0)
				{
					num3++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 20.0)
				{
					num4++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 30.0)
				{
					num5++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 40.0)
				{
					num6++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 50.0)
				{
					num7++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 100.0)
				{
					num8++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 200.0)
				{
					num9++;
				}
				if (Elements.MainAsteroids.AstElements[i].Diameter_Mean >= 400.0)
				{
					num10++;
				}
			}
			((Control)lbl5).set_Text(num.ToString());
			((Control)lbl10).set_Text(num2.ToString());
			((Control)lbl15).set_Text(num3.ToString());
			((Control)lbl20).set_Text(num4.ToString());
			((Control)lbl30).set_Text(num5.ToString());
			((Control)lbl40).set_Text(num6.ToString());
			((Control)lbl50).set_Text(num7.ToString());
			((Control)lbl100).set_Text(num8.ToString());
			((Control)lbl200).set_Text(num9.ToString());
			((Control)lbl400).set_Text(num10.ToString());
			((Control)lbl5).set_Left(((Control)opt5).get_Left() + ((Control)opt5).get_Width() / 2 - ((Control)lbl5).get_Width() / 2);
			((Control)lbl10).set_Left(((Control)opt10).get_Left() + ((Control)opt10).get_Width() / 2 - ((Control)lbl10).get_Width() / 2);
			((Control)lbl15).set_Left(((Control)opt15).get_Left() + ((Control)opt15).get_Width() / 2 - ((Control)lbl15).get_Width() / 2);
			((Control)lbl20).set_Left(((Control)opt20).get_Left() + ((Control)opt20).get_Width() / 2 - ((Control)lbl20).get_Width() / 2);
			((Control)lbl30).set_Left(((Control)opt30).get_Left() + ((Control)opt30).get_Width() / 2 - ((Control)lbl30).get_Width() / 2);
			((Control)lbl40).set_Left(((Control)opt40).get_Left() + ((Control)opt40).get_Width() / 2 - ((Control)lbl40).get_Width() / 2);
			((Control)lbl50).set_Left(((Control)opt50).get_Left() + ((Control)opt50).get_Width() / 2 - ((Control)lbl50).get_Width() / 2);
			((Control)lbl100).set_Left(((Control)opt100).get_Left() + ((Control)opt100).get_Width() / 2 - ((Control)lbl100).get_Width() / 2);
			((Control)lbl200).set_Left(((Control)opt200).get_Left() + ((Control)opt200).get_Width() / 2 - ((Control)lbl200).get_Width() / 2);
			((Control)lbl400).set_Left(((Control)opt400).get_Left() + ((Control)opt400).get_Width() / 2 - ((Control)lbl400).get_Width() / 2);
		}

		private void UserAsteroids_Edit_Resize(object sender, EventArgs e)
		{
			((Control)panelData).set_Width(((Control)this).get_Width() - 27);
			if (((Control)this).get_Height() > 440)
			{
				((Control)panelData).set_Height(((Control)this).get_Height() - 290);
			}
			((Control)lstElements).set_Height(((Control)panelData).get_Height() - 58);
		}

		private void reloadFromFileToolStripMenuItem_Click(object sender, EventArgs e)
		{
			lstElements.get_Items().Clear();
			if (IsLunar)
			{
				Elements.UserAsteroids_Lunar.Fill_AllAsteroids();
			}
			else
			{
				Elements.UserAsteroids.Fill_AllAsteroids();
			}
			UnsavedEdits = false;
			DisplayElements();
		}

		private void UserAsteroids_Edit_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_001b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0020: Unknown result type (might be due to invalid IL or missing references)
			//IL_0022: Invalid comparison between Unknown and I4
			//IL_002b: Invalid comparison between Unknown and I4
			if (UnsavedEdits)
			{
				DialogResult val = MessageBox.Show("You have made changes to the data that have not been saved.\r\n\r\nDo you want to save the changes?", "Unsaved edits", (MessageBoxButtons)3, (MessageBoxIcon)48, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				if ((int)val == 6)
				{
					Save();
				}
				if ((int)val == 2)
				{
					((CancelEventArgs)(object)e).Cancel = true;
				}
			}
		}

		private void cmdHorizons_Click(object sender, EventArgs e)
		{
			//IL_0130: Unknown result type (might be due to invalid IL or missing references)
			//IL_01c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0237: Unknown result type (might be due to invalid IL or missing references)
			//IL_0259: Unknown result type (might be due to invalid IL or missing references)
			//IL_025f: Invalid comparison between Unknown and I4
			//IL_02d4: Unknown result type (might be due to invalid IL or missing references)
			//IL_02f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ff: Invalid comparison between Unknown and I4
			//IL_0349: Unknown result type (might be due to invalid IL or missing references)
			new ConvertAstorb_etc(0);
			List<int> list = new List<int>();
			int.TryParse(((Control)txtYear).get_Text(), out var result);
			int.TryParse(((Control)txtMonth).get_Text(), out var result2);
			int.TryParse(((Control)txtDay).get_Text(), out var result3);
			int.TryParse(((Control)txtHour).get_Text(), out var result4);
			int.TryParse(((Control)txtMinute).get_Text(), out var result5);
			if (result <= 1800 || result2 <= 0 || result3 <= 0 || result4 < 0 || result5 < 0 || result2 > 12 || result3 > 31)
			{
				MessageBox.Show("The date specified is:\r\n" + ((Control)txtYear).get_Text() + ":" + ((Control)txtMonth).get_Text() + ":" + ((Control)txtDay).get_Text() + "::" + ((Control)txtHour).get_Text() + ":" + ((Control)txtMinute).get_Text() + "\r\n\r\nThis date is in error. All values must be integers, and fall within the usual range.", "Date error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			Utilities.Date_from_JD(Utilities.JD_from_Date(result, result2, result3), out result, out result2, out var _);
			if (((Control)txtAsteroids).get_Enabled())
			{
				string text = ((Control)txtAsteroids).get_Text().Trim();
				if (text.Contains(","))
				{
					string[] array = text.Split(new char[1] { ',' });
					for (int i = 0; i < array.Length; i++)
					{
						int.TryParse(array[i], out var result6);
						if (result6 < 1)
						{
							MessageBox.Show("The asteroid identified as:\r\n" + array[i] + "\r\nis in error. All asteroid numbers must be integers.", "Asteroid number error", (MessageBoxButtons)0, (MessageBoxIcon)16);
							return;
						}
						list.Add(result6);
					}
				}
				else if (text.Contains("-"))
				{
					if (text.IndexOf("-") == 0)
					{
						int.TryParse(text.Substring(1), out var result7);
						if (result7 < 1)
						{
							MessageBox.Show("The last numbered asteroid identified as:\r\n" + text.Substring(1) + "\r\nis in error. All asteroid numbers must be integers.", "Asteroid number error", (MessageBoxButtons)0, (MessageBoxIcon)16);
							return;
						}
						if (result7 > 5000 && (int)MessageBox.Show("The number of asteroids requested is >5000.\r\nIt may take a long time to retrieve them all. \r\n\r\nDo you want to proceed with the download?", "Large number of requests", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256) == 7)
						{
							return;
						}
						for (int j = 1; j <= result7; j++)
						{
							list.Add(j);
						}
					}
					else
					{
						string[] array2 = text.Split(new char[1] { '-' });
						int.TryParse(array2[0], out var result8);
						int.TryParse(array2[1], out var result9);
						if (result8 < 1)
						{
							result8 = 1;
						}
						if (result9 < 1)
						{
							MessageBox.Show("The last numbered asteroid identified as:\r\n" + array2[1] + "\r\nis in error. All asteroid numbers must be integers.", "Asteroid number error", (MessageBoxButtons)0, (MessageBoxIcon)16);
							return;
						}
						if (result9 - result8 > 5000 && (int)MessageBox.Show("The number of asteroids requested is >5000.\r\nIt may take a long time to retrieve them all. \r\n\r\nDo you want to proceed with the download?", "Large number of requests", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256) == 7)
						{
							return;
						}
						for (int k = result8; k <= result9; k++)
						{
							list.Add(k);
						}
					}
				}
				else
				{
					int.TryParse(text, out var result10);
					if (result10 < 1)
					{
						MessageBox.Show("The asteroid identified as:\r\n" + text + "\r\nis in error. The asteroid number must be an integer.", "Asteroid number error", (MessageBoxButtons)0, (MessageBoxIcon)16);
						return;
					}
					list.Add(result10);
				}
			}
			else
			{
				if (Elements.MainAsteroids.AstElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
				int num = 5;
				if (opt10.get_Checked())
				{
					num = 10;
				}
				else if (opt15.get_Checked())
				{
					num = 15;
				}
				else if (opt20.get_Checked())
				{
					num = 20;
				}
				else if (opt30.get_Checked())
				{
					num = 30;
				}
				else if (opt40.get_Checked())
				{
					num = 40;
				}
				else if (opt50.get_Checked())
				{
					num = 50;
				}
				else if (opt100.get_Checked())
				{
					num = 100;
				}
				else if (opt200.get_Checked())
				{
					num = 200;
				}
				else if (opt400.get_Checked())
				{
					num = 400;
				}
				for (int l = 0; l < Elements.MainAsteroids.AstElements.Count; l++)
				{
					if (Elements.MainAsteroids.AstElements[l].Diameter_Mean >= (double)num)
					{
						list.Add(Elements.MainAsteroids.AstElements[l].IDNumber);
					}
				}
			}
			((Control)cmdAbort).set_Visible(true);
			Abort = false;
			Cursor.set_Current(Cursors.get_WaitCursor());
			((Control)lblCurrentNum).set_Visible(true);
			for (int m = 0; m < list.Count; m++)
			{
				((Control)lblCurrentNum).set_Text(list[m].ToString());
				AsteroidElements AE = new AsteroidElements();
				if (list[m] == 134340)
				{
					continue;
				}
				http.GetHorizonsElements_TDBtime(list[m].ToString(), result, result2, result3, (double)result4 + (double)result5 / 60.0, ref AE);
				Elements.UserAsteroids.AstElements.Add(AE);
				if ((Elements.UserAsteroids.AstElements.Count % 10 == 0) | (m == list.Count - 1))
				{
					lstElements.get_Items().Clear();
					Elements.UserAsteroids.AstElements.Sort();
					for (int n = 0; n < Elements.UserAsteroids.AstElements.Count; n++)
					{
						lstElements.get_Items().Add((object)Elements.UserAsteroids.AstElements[n].UserEditString());
					}
					((ListControl)lstElements).set_SelectedIndex(Elements.UserAsteroids.AstElements.Count - 1);
					if (Elements.UserAsteroids.AstElements.Count % 50 == 0)
					{
						Save();
					}
				}
				Application.DoEvents();
				if (Abort)
				{
					break;
				}
			}
			((Control)cmdAbort).set_Visible(false);
			((Control)lblCurrentNum).set_Visible(false);
			Cursor.set_Current(Cursors.get_Default());
			Save();
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Control)pnlHorizons).set_Visible(false);
		}

		private void pnlHorizons_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 1048576)
			{
				xPos_Shape = e.get_X();
				yPos_Shape = e.get_Y();
			}
		}

		private void pnlHorizons_MouseHover(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_SizeAll());
		}

		private void optNumber_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtAsteroids).set_Enabled(optNumber.get_Checked());
		}

		private void opt50_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtAsteroids).set_Enabled(optNumber.get_Checked());
		}

		private void opt100_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtAsteroids).set_Enabled(optNumber.get_Checked());
		}

		private void opt200_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtAsteroids).set_Enabled(optNumber.get_Checked());
		}

		private void opt400_CheckedChanged(object sender, EventArgs e)
		{
			((Control)txtAsteroids).set_Enabled(optNumber.get_Checked());
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Horizons");
		}

		private void txtYear_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtMonth_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtDay_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void txtHour_KeyPress(object sender, KeyPressEventArgs e)
		{
			if (!char.IsControl(e.get_KeyChar()) && !char.IsDigit(e.get_KeyChar()))
			{
				e.set_Handled(true);
			}
		}

		private void cmdCometFromHorizons_Click(object sender, EventArgs e)
		{
			//IL_0122: Unknown result type (might be due to invalid IL or missing references)
			int.TryParse(((Control)txtYear).get_Text(), out var result);
			int.TryParse(((Control)txtMonth).get_Text(), out var result2);
			int.TryParse(((Control)txtDay).get_Text(), out var result3);
			int.TryParse(((Control)txtHour).get_Text(), out var result4);
			int.TryParse(((Control)txtMinute).get_Text(), out var result5);
			if (result <= 1800 || result2 <= 0 || result3 <= 0 || result4 < 0 || result5 < 0 || result2 > 12 || result3 > 31)
			{
				MessageBox.Show("The date specified is:\r\n" + ((Control)txtYear).get_Text() + ":" + ((Control)txtMonth).get_Text() + ":" + ((Control)txtDay).get_Text() + "::" + ((Control)txtHour).get_Text() + ":" + ((Control)txtMinute).get_Text() + "\r\n\r\nThis date is in error. All values must be integers, and fall within the usual range.", "Date error", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			foreach (object checkedItem in chkLstComets.get_CheckedItems())
			{
				string? asteroidNumber = checkedItem.ToString();
				AsteroidElements AE = new AsteroidElements();
				http.GetHorizonsElements_TDBtime(asteroidNumber, result, result2, result3, (double)result4 + (double)result5 / 60.0, ref AE);
				Elements.UserAsteroids.AstElements.Add(AE);
			}
			DisplayElements();
			Save();
		}

		private void chkLstComets_ItemCheck(object sender, ItemCheckEventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			//IL_002e: Unknown result type (might be due to invalid IL or missing references)
			CheckCount = chkLstComets.get_CheckedItems().get_Count();
			if ((int)e.get_NewValue() == 1)
			{
				CheckCount++;
			}
			if ((int)e.get_NewValue() == 0)
			{
				CheckCount--;
			}
			((Control)cmdCometFromHorizons).set_Text(string.Format("Retrieve {0,1} elements", CheckCount));
		}

		private void cmdClearElements_Click(object sender, EventArgs e)
		{
			DeleteAll();
		}

		private void cmdClearChecks_Click(object sender, EventArgs e)
		{
			for (int i = 0; i < ((ObjectCollection)chkLstComets.get_Items()).get_Count(); i++)
			{
				chkLstComets.SetItemChecked(i, false);
			}
		}

		private void cmdUpdateComets_Click(object sender, EventArgs e)
		{
			http.Download_Comet(SupressMessages: true);
		}

		private void panel3_Paint(object sender, PaintEventArgs e)
		{
		}

		private void pnlHorizons_MouseMove(object sender, MouseEventArgs e)
		{
			//IL_000e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0018: Invalid comparison between Unknown and I4
			Panel val = (Panel)((sender is Panel) ? sender : null);
			if (val != null && (int)e.get_Button() == 1048576)
			{
				((Control)val).set_Top(((Control)val).get_Top() + (e.get_Y() - yPos_Shape));
				if (((Control)val).get_Bottom() < 15)
				{
					((Control)val).set_Top(15 - ((Control)val).get_Height());
				}
				if (((Control)val).get_Top() > ((Control)this).get_Height() - 50)
				{
					((Control)val).set_Top(((Control)this).get_Height() - 50);
				}
				((Control)val).set_Left(((Control)val).get_Left() + (e.get_X() - xPos_Shape));
				if (((Control)val).get_Right() < 10)
				{
					((Control)val).set_Left(10 - ((Control)val).get_Width());
				}
				if (((Control)val).get_Left() > ((Control)this).get_Width() - 30)
				{
					((Control)val).set_Left(((Control)this).get_Width() - 30);
				}
			}
		}

		private void cmdAbort_Click(object sender, EventArgs e)
		{
			Abort = true;
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void cmdImportOELfile_Click(object sender, EventArgs eA)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0012: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Unknown result type (might be due to invalid IL or missing references)
			//IL_0015: Invalid comparison between Unknown and I4
			//IL_0018: Unknown result type (might be due to invalid IL or missing references)
			//IL_001a: Invalid comparison between Unknown and I4
			//IL_008c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0093: Expected O, but got Unknown
			//IL_00c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00cc: Invalid comparison between Unknown and I4
			//IL_012a: Unknown result type (might be due to invalid IL or missing references)
			//IL_01d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_046c: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_05e7: Expected O, but got Unknown
			//IL_0634: Unknown result type (might be due to invalid IL or missing references)
			//IL_063a: Invalid comparison between Unknown and I4
			//IL_086b: Unknown result type (might be due to invalid IL or missing references)
			DialogResult val = MessageBox.Show("IMPORTANT  To ensure all fields are filled, you need to make sure you have downloaded ASTORB recently, and have subsequently converted any of MPCORB, ASTORB, or AstDys_2 to the Occult format - making sure the asteroid size for the conversion is sufficiently small to include all asteroids in the .oel file(s).\r\n\r\n\r\n(YES)   Do you want to import from a single native OrbFit .oel file ?\r\n\r\n(NO)   Do you want to import from a file containing multiple OrbFit .oel entries in AstDyS-2 format ?", "Select file format for Import", (MessageBoxButtons)3, (MessageBoxIcon)32);
			if ((int)val == 2)
			{
				return;
			}
			if ((int)val == 6)
			{
				string text = "";
				string text2 = "";
				string text3 = "";
				string text4 = "";
				string text5 = "";
				string orbitDate = "";
				double epochDay = 1.0;
				bool flag = false;
				bool flag2 = true;
				int result = 1;
				int epochYear = 2000;
				int epochMonth = 1;
				double result2 = 0.0;
				double result3 = 0.0;
				double num = 0.0;
				string text6 = " ";
				OpenFileDialog val2 = new OpenFileDialog();
				((FileDialog)val2).set_Title("Specify file to read updated occultation elements");
				((FileDialog)val2).set_Filter("Orbital Elements file(*.oel)|*.oel|All files (*.*)|*.*");
				((FileDialog)val2).set_FilterIndex(0);
				((FileDialog)val2).set_FileName(Settings.Default.OELfile);
				if ((int)((CommonDialog)val2).ShowDialog() != 1)
				{
					return;
				}
				Settings.Default.OELfile = ((FileDialog)val2).get_FileName();
				using (StreamReader streamReader = new StreamReader(((FileDialog)val2).get_FileName()))
				{
					do
					{
						text = streamReader.ReadLine()!.PadRight(80);
						if (text.Substring(0, 6) == "refsys" && !text.Contains("2000"))
						{
							MessageBox.Show("Orbital elements are not on the J2000 equinox.\r\n\r\nNo elements will be entered.", "Error in .oel file");
							break;
						}
						if (text.Substring(0, 4) == " KEP")
						{
							text2 = text.Substring(7);
						}
						if (text.Substring(0, 4) == " MAG")
						{
							text4 = text.Substring(4);
						}
						if (text.Substring(0, 21) == "! Orbital elements of")
						{
							text3 = streamReader.ReadLine();
						}
						if (text.Substring(0, 4) == " MJD")
						{
							orbitDate = Utilities.DateShort_from_JD(2400000.5 + double.Parse(text.Substring(8, 15)));
							if (!text.Contains("TDT"))
							{
								MessageBox.Show("Elements in file are on the wrong time scale\r\nThe must be on TDT - Terrestial Dynamical Time", "Error in .oel file");
								break;
							}
						}
					}
					while (!streamReader.EndOfStream);
				}
				if (Elements.MainAsteroids.AstElements.Count < 1)
				{
					Elements.MainAsteroids.Fill_AllAsteroids();
				}
				int num2 = -1;
				string[] array = text3.Trim().Split(new char[1] { ' ' });
				if (array.Length < 2)
				{
					bool flag3;
					if (int.TryParse(array[0], out var result4))
					{
						flag3 = true;
						result = result4;
					}
					else
					{
						flag3 = false;
						text5 = array[0].Trim().ToLower().PadRight(15);
						if (int.TryParse(text5.Substring(0, 4), out result4))
						{
							text5 = text5.Replace(" ", "").Insert(4, " ").PadRight(15)
								.Substring(0, 15);
						}
					}
					num2 = ((!flag3) ? Elements.MainAsteroids.GetAsteroidRecord_fromName(text5) : Elements.MainAsteroids.GetAsteroidRecord_fromNumber(result));
					if (num2 >= 0)
					{
						result = Elements.MainAsteroids.AstElements[num2].IDNumber;
						text5 = Elements.MainAsteroids.AstElements[num2].IDName;
					}
				}
				else
				{
					result = int.Parse(array[0]);
					text5 = array[1];
				}
				if (num2 >= 0)
				{
					text2 = text2.PadRight(144);
				}
				AsteroidElements asteroidElements = new AsteroidElements();
				if (!int.TryParse(text3, out result))
				{
					result = 0;
				}
				flag2 &= double.TryParse(text2.Substring(95, 14), out var result5);
				flag2 &= double.TryParse(text2.Substring(77, 14), out var result6);
				flag2 &= double.TryParse(text2.Substring(59, 14), out var result7);
				flag2 &= double.TryParse(text2.Substring(41, 14), out var result8);
				flag2 &= double.TryParse(text2.Substring(23, 14), out var result9);
				flag2 &= double.TryParse(text2.Substring(0, 21), out var result10);
				flag2 &= double.TryParse(text4.Substring(0, 8), out var result11);
				flag2 &= double.TryParse(text4.Substring(8, 7), out var result12);
				if (!flag)
				{
					result2 = (result3 = 10.0);
					text6 = " 0";
				}
				else
				{
					flag2 &= double.TryParse(text.Substring(119, 7), out result2);
					flag2 &= double.TryParse(text.Substring(126, 7), out result3) | (text.Substring(126, 7).Trim() == "");
					text6 = text.Substring(133, 2);
				}
				num = 0.05;
				if (!flag2)
				{
					MessageBox.Show("One or more data fields are invalid", "Data input error", (MessageBoxButtons)0, (MessageBoxIcon)16);
					return;
				}
				asteroidElements.IDNumber = result;
				asteroidElements.IDName = text5;
				asteroidElements.Meananomaly = result5;
				asteroidElements.EpochYear = epochYear;
				asteroidElements.EpochMonth = epochMonth;
				asteroidElements.EpochDay = epochDay;
				asteroidElements.Perihelion = result6;
				asteroidElements.Node = result7;
				asteroidElements.I = result8;
				asteroidElements.E = result9;
				asteroidElements.A = result10;
				asteroidElements.H0 = result11;
				asteroidElements.G_phaseCoeff = result12;
				asteroidElements.Diameter_Mean = result2;
				asteroidElements.Dia_Source = text6;
				asteroidElements.PeakEphemUncert = num;
				ConvertAstorb_etc convertAstorb_etc = new ConvertAstorb_etc(0);
				asteroidElements.Num_Rings = AsteroidRings_All.GetNumberOfRings(result);
				if (BinaryAsteroids.BinElements.Count < 1)
				{
					BinaryAsteroids.Fill_AllAsteroids();
				}
				asteroidElements.Num_Moons = convertAstorb_etc.GetMoons(result, text5);
				asteroidElements.OrbitSource = AsteroidElements.Sources[4];
				asteroidElements.OrbitDate = orbitDate;
				Elements.UserAsteroids.AstElements.Add(asteroidElements);
				lstElements.get_Items().Add((object)asteroidElements.ToString());
				Elements.UserAsteroids.AstElements.Sort();
				DisplayElements();
				((ListControl)lstElements).set_SelectedIndex(lstElements.get_Items().get_Count() - 1);
				UnsavedEdits = true;
				return;
			}
			ConvertAstorb_etc convertAstorb_etc2 = new ConvertAstorb_etc(0);
			if (AsteroidClassList.AClass.Count < 1)
			{
				AsteroidClassList.Fill_AllAsteroids();
			}
			int i = 0;
			int count = AsteroidClassList.AClass.Count;
			OpenFileDialog val3 = new OpenFileDialog();
			((FileDialog)val3).set_CheckPathExists(true);
			((FileDialog)val3).set_FileName("catalog.dat");
			((FileDialog)val3).set_Title("Add from Orbfit file in AstDys-2 format");
			((FileDialog)val3).set_InitialDirectory(AppPath + "\\Downloaded Files\\");
			((FileDialog)val3).set_Filter("catalog*.dat|catalog*.dat|All files (*.*)|*.*");
			((FileDialog)val3).set_FilterIndex(1);
			if ((int)((CommonDialog)val3).ShowDialog() != 1)
			{
				return;
			}
			Utilities.OpenAsteroidDiameterFileForReading();
			ConvertAstorb_etc.BinaryRecNum = 0;
			using (StreamReader streamReader2 = new StreamReader(((FileDialog)val3).get_FileName(), detectEncodingFromByteOrderMarks: true))
			{
				AsteroidElements asteroidElements2 = new AsteroidElements();
				AsteroidElements_All asteroidElements_All = new AsteroidElements_All(Utilities.AppPath + "\\Resource Files\\AsteroidElements.csv");
				asteroidElements_All.Fill_AllAsteroids();
				for (int j = 0; j < 6; j++)
				{
					string text7 = streamReader2.ReadLine();
				}
				do
				{
					string text7 = streamReader2.ReadLine();
					string orbitDate2 = Utilities.DateShort_from_JD(2400000.5 + double.Parse(text7.Substring(12, 15)));
					asteroidElements2 = new AsteroidElements();
					asteroidElements2.ReadAstDySLines(text7);
					asteroidElements2.IDName = asteroidElements_All.GetAsteroidName_fromNumber(asteroidElements2.IDNumber, out var PeakUncertaintyError);
					asteroidElements2.PeakEphemUncert = PeakUncertaintyError;
					asteroidElements2.OrbitSource = AsteroidElements.Sources[4];
					Utilities.GetAsteroidDiameter(asteroidElements2.IDNumber, asteroidElements2.H0, out var MeanDiameter, out var DiameterUncertainty, out var DiaSource, out var _);
					asteroidElements2.Diameter_Mean = MeanDiameter;
					asteroidElements2.DiameterUncertainty = DiameterUncertainty;
					asteroidElements2.Dia_Source = DiaSource;
					asteroidElements2.Num_Rings = AsteroidRings_All.GetNumberOfRings(asteroidElements2.IDNumber);
					asteroidElements2.Num_Moons = convertAstorb_etc2.GetMoons(asteroidElements2.IDNumber, asteroidElements2.IDName);
					asteroidElements2.OrbitDate = orbitDate2;
					if (i < count)
					{
						for (; AsteroidClassList.AClass[i].Number < asteroidElements2.IDNumber && i < count - 1; i++)
						{
						}
						if (AsteroidClassList.AClass[i].Number == asteroidElements2.IDNumber)
						{
							asteroidElements2.AsteroidClass = AsteroidClassList.AClass[i].AstClass;
						}
					}
					Elements.UserAsteroids.AstElements.Add(asteroidElements2);
					lstElements.get_Items().Add((object)asteroidElements2.ToString());
					Elements.UserAsteroids.AstElements.Sort();
					DisplayElements();
					((ListControl)lstElements).set_SelectedIndex(lstElements.get_Items().get_Count() - 1);
					UnsavedEdits = true;
				}
				while (!streamReader2.EndOfStream);
			}
			Utilities.CloseAsteroidDiameterFileForReading();
			convertAstorb_etc2.AsteroidRings.Close();
			MessageBox.Show("OrbFit conversion complete", "OrbFit conversion complete", (MessageBoxButtons)0, (MessageBoxIcon)64);
			Elements.MainAsteroids.AstElements.Clear();
			Elements.UserAsteroids_Lunar.AstElements.Clear();
			AsteroidClassList.AClass.Clear();
		}

		internal bool CreateCometObsList()
		{
			//IL_0063: Unknown result type (might be due to invalid IL or missing references)
			//IL_0069: Invalid comparison between Unknown and I4
			CometList.Clear();
			if (!File.Exists(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt"))
			{
				return false;
			}
			if (Directory.GetLastWriteTime(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt") < DateTime.Now.AddMonths(-1) && (int)MessageBox.Show("The file of dates of last comet observations is more than 1 month old.\r\nFor a reliable selection of comets, you should do a fresh download (which occurs as part of the download of the Comet orbital elements).\r\n\r\nDo you want to continue, using the existing 'old' file?", "Old file", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return false;
			}
			using (StreamReader streamReader = new StreamReader(Utilities.AppPath + "/Downloaded Files/CometLastObs.txt"))
			{
				streamReader.ReadLine();
				do
				{
					string text = streamReader.ReadLine();
					if (text.Length > 54)
					{
						CometLast = new CometLastObs();
						CometLast.DecodeCometLastObs(text);
						CometList.Add(CometLast);
					}
				}
				while (!streamReader.EndOfStream);
			}
			CometList.Sort();
			return true;
		}

		private void cmdImportAllCurrentComets_Click(object sender, EventArgs e)
		{
			//IL_0015: Unknown result type (might be due to invalid IL or missing references)
			if (!CreateCometObsList())
			{
				MessageBox.Show("The file of Last Observations of Comets has not been downloaded.\r\n\r\nThis file is downloaded when Comet.dat is downloaded.\r\n\r\nDownload Comet.dat, and try again.", "Last observations not available", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			bool flag = false;
			int num = 0;
			int num2 = 0;
			float num3 = 14f;
			if (opt18.get_Checked())
			{
				num3 = 18f;
			}
			if (optAll.get_Checked())
			{
				num3 = 30f;
			}
			for (int i = 0; i < CometElements.Count; i++)
			{
				string text = CometElements[i]!.ToString()!.Substring(102);
				num = text.IndexOf("P-");
				if (num > 0)
				{
					num += 2;
				}
				if (num < 0)
				{
					num = text.IndexOf("P/");
				}
				if (num < 0)
				{
					num = text.IndexOf("P ");
				}
				string text2;
				if (num > 0 && num < 7)
				{
					text2 = text.Substring(0, num + 1).PadLeft(4);
				}
				else
				{
					num2 = text.IndexOf("(");
					text2 = ((num2 <= 0) ? text.Substring(2, 33).Trim() : text.Substring(2, num2 - 2).Trim());
				}
				int num4 = 0;
				int num5 = CometList.Count - 1;
				int num6 = 0;
				flag = false;
				do
				{
					num6 = (num4 + num5) / 2;
					if (text2 == CometList[num6].ID)
					{
						flag = true;
						break;
					}
					if (text2.CompareTo(CometList[num6].ID) < 1)
					{
						num5 = num6 - 1;
					}
					else
					{
						num4 = num6 + 1;
					}
				}
				while (num4 <= num5);
				if (flag && !(CometList[num6].LastObsDate < DateTime.Now.AddMonths(-6)) && !(CometList[num6].LastMag > num3))
				{
					ImportElementsOfAComet(i);
				}
			}
			DisplayElements();
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
			//IL_320d: Unknown result type (might be due to invalid IL or missing references)
			//IL_3217: Expected O, but got Unknown
			//IL_323b: Unknown result type (might be due to invalid IL or missing references)
			//IL_3245: Expected O, but got Unknown
			//IL_339f: Unknown result type (might be due to invalid IL or missing references)
			//IL_33a9: Expected O, but got Unknown
			//IL_34b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_34bf: Expected O, but got Unknown
			//IL_478e: Unknown result type (might be due to invalid IL or missing references)
			//IL_4798: Expected O, but got Unknown
			//IL_4b1d: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b27: Expected O, but got Unknown
			//IL_4b7b: Unknown result type (might be due to invalid IL or missing references)
			//IL_4b85: Expected O, but got Unknown
			//IL_4bd6: Unknown result type (might be due to invalid IL or missing references)
			//IL_4be0: Expected O, but got Unknown
			//IL_50b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_50be: Expected O, but got Unknown
			//IL_512d: Unknown result type (might be due to invalid IL or missing references)
			//IL_5137: Expected O, but got Unknown
			//IL_513f: Unknown result type (might be due to invalid IL or missing references)
			//IL_5149: Expected O, but got Unknown
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(UserAsteroids_Edit));
			lstElements = new ListBox();
			label1 = new Label();
			label2 = new Label();
			cmdDeleteAll = new Button();
			cmdDeleteSelected = new Button();
			cmdAddFromFile = new Button();
			cmdAddH0Limited = new Button();
			groupOppMag = new GroupBox();
			label19 = new Label();
			label3 = new Label();
			updnMag = new NumericUpDown();
			cmdSave = new Button();
			txtNumber = new TextBox();
			txtName = new TextBox();
			txtMA = new TextBox();
			updnYear = new NumericUpDown();
			updnMonth = new NumericUpDown();
			updnDay = new NumericUpDown();
			txtA = new TextBox();
			txte = new TextBox();
			txtI = new TextBox();
			txtNode = new TextBox();
			txtPerihelion = new TextBox();
			txtError = new TextBox();
			txtMaxD = new TextBox();
			txtG = new TextBox();
			txtH0 = new TextBox();
			label4 = new Label();
			label5 = new Label();
			label7 = new Label();
			label8 = new Label();
			label9 = new Label();
			label10 = new Label();
			label11 = new Label();
			label12 = new Label();
			label13 = new Label();
			label14 = new Label();
			label15 = new Label();
			label16 = new Label();
			label17 = new Label();
			grpEdit = new GroupBox();
			label6 = new Label();
			cmbAsteroidClasses = new ComboBox();
			label25 = new Label();
			txtOrbitDate = new TextBox();
			label24 = new Label();
			txtOrbitSrc = new TextBox();
			label23 = new Label();
			cmbRings = new ComboBox();
			label21 = new Label();
			label20 = new Label();
			cmbMoons = new ComboBox();
			cmdCancel = new Button();
			cmdAddElements = new Button();
			cmdReplaceSelected = new Button();
			label18 = new Label();
			cmbSource = new ComboBox();
			cmdEdit = new Button();
			panelMain = new Panel();
			groupBox1 = new GroupBox();
			cmdImportOELfile = new Button();
			grpComets = new GroupBox();
			cmdUpdateComets = new Button();
			panel1 = new Panel();
			cmdImportAllCurrentComets = new Button();
			label22 = new Label();
			optAll = new RadioButton();
			opt18 = new RadioButton();
			opt14 = new RadioButton();
			cmbComet = new ComboBox();
			cmdImportComet = new Button();
			menuStrip1 = new MenuStrip();
			fileToolStripMenuItem = new ToolStripMenuItem();
			reloadFromFileToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			pasteElementsFromJPLHorizonsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			panelData = new Panel();
			pnlHorizons = new Panel();
			cmdClearElements = new Button();
			panel3 = new Panel();
			cmdClearChecks = new Button();
			chkLstComets = new CheckedListBox();
			cmdCometFromHorizons = new Button();
			label37 = new Label();
			panelAsteroid = new Panel();
			label33 = new Label();
			lbl20 = new Label();
			opt20 = new RadioButton();
			lbl30 = new Label();
			opt30 = new RadioButton();
			lbl40 = new Label();
			opt40 = new RadioButton();
			lbl400 = new Label();
			lbl200 = new Label();
			lbl100 = new Label();
			lbl50 = new Label();
			opt100 = new RadioButton();
			opt200 = new RadioButton();
			opt400 = new RadioButton();
			optNumber = new RadioButton();
			opt50 = new RadioButton();
			lblCurrentNum = new Label();
			cmdAbort = new Button();
			cmdHorizons = new Button();
			label31 = new Label();
			txtAsteroids = new TextBox();
			label30 = new Label();
			panel2 = new Panel();
			label36 = new Label();
			label35 = new Label();
			txtMinute = new TextBox();
			label34 = new Label();
			txtHour = new TextBox();
			cmdHelp = new Button();
			label32 = new Label();
			cmdExit = new Button();
			label29 = new Label();
			label28 = new Label();
			label27 = new Label();
			label26 = new Label();
			txtMonth = new TextBox();
			txtDay = new TextBox();
			txtYear = new TextBox();
			cmbCometHorizons = new ComboBox();
			lbl5 = new Label();
			opt5 = new RadioButton();
			lbl10 = new Label();
			opt10 = new RadioButton();
			lbl15 = new Label();
			opt15 = new RadioButton();
			label38 = new Label();
			((Control)groupOppMag).SuspendLayout();
			((ISupportInitialize)updnMag).BeginInit();
			((ISupportInitialize)updnYear).BeginInit();
			((ISupportInitialize)updnMonth).BeginInit();
			((ISupportInitialize)updnDay).BeginInit();
			((Control)grpEdit).SuspendLayout();
			((Control)panelMain).SuspendLayout();
			((Control)groupBox1).SuspendLayout();
			((Control)grpComets).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)panelData).SuspendLayout();
			((Control)pnlHorizons).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)panelAsteroid).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstElements).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstElements).set_FormattingEnabled(true);
			lstElements.set_ItemHeight(14);
			((Control)lstElements).set_Location(new Point(4, 36));
			((Control)lstElements).set_Name("lstElements");
			((Control)lstElements).set_Size(new Size(1317, 270));
			((Control)lstElements).set_TabIndex(2);
			lstElements.add_SelectedIndexChanged((EventHandler)lstElements_SelectedIndexChanged);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(5, 10));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(1316, 14));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("                          epoch [or T]      M.A.       perihelion     node      inclination    e              a [or q]     Magnitudes  diam      src Err  # # Orbit     Orbit     Taxonomic");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(5, 21));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(1289, 14));
			((Control)label2).set_TabIndex(1);
			((Control)label2).set_Text(" number Name                y m  d           o            o            o            o                                       H0    G     km           \"    ⊚ ☾ src       date      class");
			((Control)cmdDeleteAll).set_BackColor(Color.Pink);
			((Control)cmdDeleteAll).set_Location(new Point(734, 48));
			((Control)cmdDeleteAll).set_Name("cmdDeleteAll");
			((Control)cmdDeleteAll).set_Size(new Size(128, 32));
			((Control)cmdDeleteAll).set_TabIndex(4);
			((Control)cmdDeleteAll).set_Text("Delete ALL from list");
			((ButtonBase)cmdDeleteAll).set_UseVisualStyleBackColor(false);
			((Control)cmdDeleteAll).add_Click((EventHandler)cmdDeleteAll_Click);
			((Control)cmdDeleteSelected).set_Location(new Point(734, 10));
			((Control)cmdDeleteSelected).set_Name("cmdDeleteSelected");
			((Control)cmdDeleteSelected).set_Size(new Size(128, 32));
			((Control)cmdDeleteSelected).set_TabIndex(3);
			((Control)cmdDeleteSelected).set_Text("Delete selected from list");
			((ButtonBase)cmdDeleteSelected).set_UseVisualStyleBackColor(true);
			((Control)cmdDeleteSelected).add_Click((EventHandler)cmdDeleteSelected_Click);
			((Control)cmdAddFromFile).set_Location(new Point(5, 17));
			((Control)cmdAddFromFile).set_Name("cmdAddFromFile");
			((Control)cmdAddFromFile).set_Size(new Size(115, 31));
			((Control)cmdAddFromFile).set_TabIndex(0);
			((Control)cmdAddFromFile).set_Text("Import from main file");
			((ButtonBase)cmdAddFromFile).set_UseVisualStyleBackColor(true);
			((Control)cmdAddFromFile).add_Click((EventHandler)cmdAddFromFile_Click);
			((Control)cmdAddH0Limited).set_Location(new Point(212, 25));
			((Control)cmdAddH0Limited).set_Name("cmdAddH0Limited");
			((Control)cmdAddH0Limited).set_Size(new Size(77, 32));
			((Control)cmdAddH0Limited).set_TabIndex(3);
			((Control)cmdAddH0Limited).set_Text("Add");
			((ButtonBase)cmdAddH0Limited).set_UseVisualStyleBackColor(true);
			((Control)cmdAddH0Limited).add_Click((EventHandler)cmdAddH0Limited_Click);
			((Control)groupOppMag).get_Controls().Add((Control)(object)label19);
			((Control)groupOppMag).get_Controls().Add((Control)(object)label3);
			((Control)groupOppMag).get_Controls().Add((Control)(object)updnMag);
			((Control)groupOppMag).get_Controls().Add((Control)(object)cmdAddH0Limited);
			((Control)groupOppMag).set_Location(new Point(5, 5));
			((Control)groupOppMag).set_Name("groupOppMag");
			((Control)groupOppMag).set_Size(new Size(300, 80));
			((Control)groupOppMag).set_TabIndex(0);
			groupOppMag.set_TabStop(false);
			((Control)groupOppMag).set_Text("For Lunar occultations");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(5, 64));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(296, 13));
			((Control)label19).set_TabIndex(1);
			((Control)label19).set_Text("NOTE - adding many asteroids will slow predictions");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(5, 17));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(136, 39));
			((Control)label3).set_TabIndex(0);
			((Control)label3).set_Text("Add all minor planets where\r\nthe mean opposition\r\nmagnitude is brighter than:");
			updnMag.set_DecimalPlaces(1);
			updnMag.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMag).set_Location(new Point(151, 31));
			updnMag.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMag.set_Minimum(new decimal(new int[4] { 7, 0, 0, 0 }));
			((Control)updnMag).set_Name("updnMag");
			((Control)updnMag).set_Size(new Size(44, 20));
			((Control)updnMag).set_TabIndex(2);
			updnMag.set_Value(new decimal(new int[4] { 9, 0, 0, 0 }));
			((Control)updnMag).add_Enter((EventHandler)updnMag_Enter);
			((Control)cmdSave).set_Location(new Point(865, 48));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(148, 32));
			((Control)cmdSave).set_TabIndex(6);
			((Control)cmdSave).set_Text("Save list");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(true);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((Control)txtNumber).set_Location(new Point(52, 19));
			((TextBoxBase)txtNumber).set_MaxLength(7);
			((Control)txtNumber).set_Name("txtNumber");
			((Control)txtNumber).set_Size(new Size(63, 20));
			((Control)txtNumber).set_TabIndex(1);
			((Control)txtNumber).add_Enter((EventHandler)txtNumber_Enter);
			((Control)txtName).set_Location(new Point(52, 45));
			((TextBoxBase)txtName).set_MaxLength(16);
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(169, 20));
			((Control)txtName).set_TabIndex(3);
			((Control)txtName).add_Enter((EventHandler)txtName_Enter);
			((Control)txtMA).set_Location(new Point(254, 19));
			((Control)txtMA).set_Name("txtMA");
			((Control)txtMA).set_Size(new Size(84, 20));
			((Control)txtMA).set_TabIndex(9);
			((Control)txtMA).add_Enter((EventHandler)txtMA_Enter);
			((Control)updnYear).set_Location(new Point(52, 75));
			updnYear.set_Maximum(new decimal(new int[4] { 2100, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(48, 20));
			((Control)updnYear).set_TabIndex(5);
			updnYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			((Control)updnYear).add_Enter((EventHandler)updnYear_Enter);
			((Control)updnMonth).set_Location(new Point(105, 75));
			updnMonth.set_Maximum(new decimal(new int[4] { 12, 0, 0, 0 }));
			updnMonth.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).set_Name("updnMonth");
			((Control)updnMonth).set_Size(new Size(35, 20));
			((Control)updnMonth).set_TabIndex(6);
			updnMonth.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnMonth).add_Enter((EventHandler)updnMonth_Enter);
			updnDay.set_DecimalPlaces(8);
			((Control)updnDay).set_Location(new Point(145, 75));
			updnDay.set_Maximum(new decimal(new int[4] { 319999999, 0, 0, 458752 }));
			updnDay.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).set_Name("updnDay");
			((Control)updnDay).set_Size(new Size(76, 20));
			((Control)updnDay).set_TabIndex(7);
			updnDay.set_Value(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDay).add_Enter((EventHandler)updnDay_Enter);
			((Control)txtA).set_Location(new Point(254, 75));
			((Control)txtA).set_Name("txtA");
			((Control)txtA).set_Size(new Size(84, 20));
			((Control)txtA).set_TabIndex(13);
			((Control)txtA).add_Enter((EventHandler)txtA_Enter);
			((Control)txte).set_Location(new Point(254, 45));
			((Control)txte).set_Name("txte");
			((Control)txte).set_Size(new Size(84, 20));
			((Control)txte).set_TabIndex(11);
			((Control)txte).add_Enter((EventHandler)txte_Enter);
			((Control)txtI).set_Location(new Point(402, 75));
			((Control)txtI).set_Name("txtI");
			((Control)txtI).set_Size(new Size(84, 20));
			((Control)txtI).set_TabIndex(19);
			((Control)txtI).add_Enter((EventHandler)txtI_Enter);
			((Control)txtNode).set_Location(new Point(402, 45));
			((Control)txtNode).set_Name("txtNode");
			((Control)txtNode).set_Size(new Size(84, 20));
			((Control)txtNode).set_TabIndex(17);
			((Control)txtNode).add_Enter((EventHandler)txtNode_Enter);
			((Control)txtPerihelion).set_Location(new Point(402, 19));
			((Control)txtPerihelion).set_Name("txtPerihelion");
			((Control)txtPerihelion).set_Size(new Size(84, 20));
			((Control)txtPerihelion).set_TabIndex(15);
			((Control)txtPerihelion).add_Enter((EventHandler)txtPerihelion_Enter);
			((Control)txtError).set_Location(new Point(567, 78));
			((Control)txtError).set_Name("txtError");
			((Control)txtError).set_Size(new Size(36, 20));
			((Control)txtError).set_TabIndex(25);
			((Control)txtError).set_Text("0.50");
			((Control)txtError).add_Enter((EventHandler)txtError_Enter);
			((Control)txtMaxD).set_Location(new Point(642, 19));
			((Control)txtMaxD).set_Name("txtMaxD");
			((Control)txtMaxD).set_Size(new Size(41, 20));
			((Control)txtMaxD).set_TabIndex(27);
			((Control)txtMaxD).add_Enter((EventHandler)txtMaxD_Enter);
			((Control)txtG).set_Location(new Point(553, 45));
			((Control)txtG).set_Name("txtG");
			((Control)txtG).set_Size(new Size(33, 20));
			((Control)txtG).set_TabIndex(23);
			((Control)txtG).set_Text("0.15");
			((Control)txtG).add_Enter((EventHandler)txtG_Enter);
			((Control)txtH0).set_Location(new Point(553, 19));
			((Control)txtH0).set_Name("txtH0");
			((Control)txtH0).set_Size(new Size(34, 20));
			((Control)txtH0).set_TabIndex(21);
			((Control)txtH0).add_Enter((EventHandler)txtH0_Enter);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(5, 23));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(44, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("Number");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(495, 72));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(70, 26));
			((Control)label5).set_TabIndex(24);
			((Control)label5).set_Text("positional\r\nuncertainty(\")");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(239, 79));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(13, 13));
			((Control)label7).set_TabIndex(12);
			((Control)label7).set_Text("a");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Location(new Point(239, 49));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(13, 13));
			((Control)label8).set_TabIndex(10);
			((Control)label8).set_Text("e");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(175, 23));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(77, 13));
			((Control)label9).set_TabIndex(8);
			((Control)label9).set_Text("Mean Anomaly");
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(11, 71));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(38, 26));
			((Control)label10).set_TabIndex(4);
			((Control)label10).set_Text("Epoch\r\n(y m d)");
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Location(new Point(14, 49));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(35, 13));
			((Control)label11).set_TabIndex(2);
			((Control)label11).set_Text("Name");
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(530, 49));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(15, 13));
			((Control)label12).set_TabIndex(22);
			((Control)label12).set_Text("G");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(494, 23));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(56, 13));
			((Control)label13).set_TabIndex(20);
			((Control)label13).set_Text("Mags   H0");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(390, 79));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(9, 13));
			((Control)label14).set_TabIndex(18);
			((Control)label14).set_Text("i");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(369, 49));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(31, 13));
			((Control)label15).set_TabIndex(16);
			((Control)label15).set_Text("node");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Location(new Point(348, 23));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(52, 13));
			((Control)label16).set_TabIndex(14);
			((Control)label16).set_Text("perihelion");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Location(new Point(594, 23));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(47, 13));
			((Control)label17).set_TabIndex(26);
			((Control)label17).set_Text("diameter");
			((Control)grpEdit).set_Anchor((AnchorStyles)6);
			((Control)grpEdit).get_Controls().Add((Control)(object)label6);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmbAsteroidClasses);
			((Control)grpEdit).get_Controls().Add((Control)(object)label25);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtOrbitDate);
			((Control)grpEdit).get_Controls().Add((Control)(object)label24);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtOrbitSrc);
			((Control)grpEdit).get_Controls().Add((Control)(object)label23);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmbRings);
			((Control)grpEdit).get_Controls().Add((Control)(object)label21);
			((Control)grpEdit).get_Controls().Add((Control)(object)label20);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmbMoons);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdCancel);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdAddElements);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdReplaceSelected);
			((Control)grpEdit).get_Controls().Add((Control)(object)label18);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmbSource);
			((Control)grpEdit).get_Controls().Add((Control)(object)label17);
			((Control)grpEdit).get_Controls().Add((Control)(object)label16);
			((Control)grpEdit).get_Controls().Add((Control)(object)label15);
			((Control)grpEdit).get_Controls().Add((Control)(object)label14);
			((Control)grpEdit).get_Controls().Add((Control)(object)label13);
			((Control)grpEdit).get_Controls().Add((Control)(object)label12);
			((Control)grpEdit).get_Controls().Add((Control)(object)label11);
			((Control)grpEdit).get_Controls().Add((Control)(object)label10);
			((Control)grpEdit).get_Controls().Add((Control)(object)label9);
			((Control)grpEdit).get_Controls().Add((Control)(object)label8);
			((Control)grpEdit).get_Controls().Add((Control)(object)label7);
			((Control)grpEdit).get_Controls().Add((Control)(object)label5);
			((Control)grpEdit).get_Controls().Add((Control)(object)label4);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtH0);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtG);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtMaxD);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtError);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtPerihelion);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtNode);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtI);
			((Control)grpEdit).get_Controls().Add((Control)(object)txte);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtA);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnDay);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnMonth);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnYear);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtMA);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtName);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtNumber);
			((Control)grpEdit).set_Enabled(false);
			((Control)grpEdit).set_Location(new Point(6, 459));
			((Control)grpEdit).set_Name("grpEdit");
			((Control)grpEdit).set_Size(new Size(1014, 117));
			((Control)grpEdit).set_TabIndex(3);
			grpEdit.set_TabStop(false);
			((Control)grpEdit).set_Text("Manually edit the elements");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(641, 74));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(82, 13));
			((Control)label6).set_TabIndex(34);
			((Control)label6).set_Text("Taxonomic class");
			((ListControl)cmbAsteroidClasses).set_FormattingEnabled(true);
			cmbAsteroidClasses.get_Items().AddRange(new object[8] { "", "Amor", "Apollo", "Aten", "Centaur", "PHA", "TNO", "Trojan" });
			((Control)cmbAsteroidClasses).set_Location(new Point(724, 70));
			((Control)cmbAsteroidClasses).set_Name("cmbAsteroidClasses");
			((Control)cmbAsteroidClasses).set_Size(new Size(76, 21));
			((Control)cmbAsteroidClasses).set_TabIndex(35);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(731, 97));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(28, 13));
			((Control)label25).set_TabIndex(38);
			((Control)label25).set_Text("date");
			((Control)txtOrbitDate).set_Location(new Point(760, 94));
			((Control)txtOrbitDate).set_Name("txtOrbitDate");
			((Control)txtOrbitDate).set_Size(new Size(64, 20));
			((Control)txtOrbitDate).set_TabIndex(39);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(588, 99));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(62, 13));
			((Control)label24).set_TabIndex(36);
			((Control)label24).set_Text("orbit source");
			((Control)txtOrbitSrc).set_Location(new Point(651, 94));
			((Control)txtOrbitSrc).set_Name("txtOrbitSrc");
			((Control)txtOrbitSrc).set_Size(new Size(64, 20));
			((Control)txtOrbitSrc).set_TabIndex(37);
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Location(new Point(597, 49));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(39, 13));
			((Control)label23).set_TabIndex(30);
			((Control)label23).set_Text("# rings");
			cmbRings.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbRings).set_FormattingEnabled(true);
			cmbRings.get_Items().AddRange(new object[10] { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" });
			((Control)cmbRings).set_Location(new Point(641, 45));
			((Control)cmbRings).set_Name("cmbRings");
			((Control)cmbRings).set_Size(new Size(42, 21));
			((Control)cmbRings).set_TabIndex(31);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(41, 100));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(498, 13));
			((Control)label21).set_TabIndex(43);
			((Control)label21).set_Text("For objects with e>.97, set Epoch = perihelion date (T), Mean Anomaly=0, and a = perihelion distance (q)");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(695, 49));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(48, 13));
			((Control)label20).set_TabIndex(32);
			((Control)label20).set_Text("# moons");
			cmbMoons.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbMoons).set_FormattingEnabled(true);
			cmbMoons.get_Items().AddRange(new object[10] { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" });
			((Control)cmbMoons).set_Location(new Point(744, 45));
			((Control)cmbMoons).set_Name("cmbMoons");
			((Control)cmbMoons).set_Size(new Size(42, 21));
			((Control)cmbMoons).set_TabIndex(33);
			((Control)cmdCancel).set_Location(new Point(939, 23));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(67, 35));
			((Control)cmdCancel).set_TabIndex(42);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdAddElements).set_Location(new Point(832, 61));
			((Control)cmdAddElements).set_Name("cmdAddElements");
			((Control)cmdAddElements).set_Size(new Size(100, 35));
			((Control)cmdAddElements).set_TabIndex(41);
			((Control)cmdAddElements).set_Text("Add as a new object");
			((ButtonBase)cmdAddElements).set_UseVisualStyleBackColor(true);
			((Control)cmdAddElements).add_Click((EventHandler)cmdAddElements_Click);
			((Control)cmdReplaceSelected).set_Location(new Point(832, 23));
			((Control)cmdReplaceSelected).set_Name("cmdReplaceSelected");
			((Control)cmdReplaceSelected).set_Size(new Size(100, 35));
			((Control)cmdReplaceSelected).set_TabIndex(40);
			((Control)cmdReplaceSelected).set_Text("Replace selected elements");
			((ButtonBase)cmdReplaceSelected).set_UseVisualStyleBackColor(true);
			((Control)cmdReplaceSelected).add_Click((EventHandler)cmdReplaceSelected_Click);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(687, 23));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(39, 13));
			((Control)label18).set_TabIndex(28);
			((Control)label18).set_Text("source");
			cmbSource.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbSource).set_FormattingEnabled(true);
			cmbSource.get_Items().AddRange(new object[17]
			{
				"Unknown", "IRAS mimpsalb", "IRAS diamalb", "IRAS single", "Richmond", "Photometry", "from H0", "Occultation", "Direct imaging", "Miscellaneous",
				"Akari AcuA", "WISE", "-", "-", "-", "-", "Mean of IR sats"
			});
			((Control)cmbSource).set_Location(new Point(726, 20));
			((Control)cmbSource).set_Name("cmbSource");
			((Control)cmbSource).set_Size(new Size(95, 21));
			((Control)cmbSource).set_TabIndex(29);
			((Control)cmdEdit).set_Location(new Point(865, 10));
			((Control)cmdEdit).set_Name("cmdEdit");
			((Control)cmdEdit).set_Size(new Size(148, 32));
			((Control)cmdEdit).set_TabIndex(5);
			((Control)cmdEdit).set_Text("Manually Add/Edit elements");
			((ButtonBase)cmdEdit).set_UseVisualStyleBackColor(true);
			((Control)cmdEdit).add_Click((EventHandler)cmdEdit_Click);
			((Control)panelMain).set_Anchor((AnchorStyles)6);
			((Control)panelMain).get_Controls().Add((Control)(object)groupBox1);
			((Control)panelMain).get_Controls().Add((Control)(object)grpComets);
			((Control)panelMain).get_Controls().Add((Control)(object)cmdEdit);
			((Control)panelMain).get_Controls().Add((Control)(object)cmdSave);
			((Control)panelMain).get_Controls().Add((Control)(object)groupOppMag);
			((Control)panelMain).get_Controls().Add((Control)(object)cmdDeleteSelected);
			((Control)panelMain).get_Controls().Add((Control)(object)cmdDeleteAll);
			((Control)panelMain).set_Location(new Point(6, 365));
			((Control)panelMain).set_Name("panelMain");
			((Control)panelMain).set_Size(new Size(1028, 94));
			((Control)panelMain).set_TabIndex(2);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdImportOELfile);
			((Control)groupBox1).get_Controls().Add((Control)(object)cmdAddFromFile);
			((Control)groupBox1).set_Location(new Point(312, 5));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(123, 79));
			((Control)groupBox1).set_TabIndex(1);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Import ASTEROIDS");
			((Control)cmdImportOELfile).set_Location(new Point(4, 52));
			((Control)cmdImportOELfile).set_Name("cmdImportOELfile");
			((Control)cmdImportOELfile).set_Size(new Size(116, 25));
			((Control)cmdImportOELfile).set_TabIndex(1);
			((Control)cmdImportOELfile).set_Text("Import OrbFit .oel files");
			((ButtonBase)cmdImportOELfile).set_UseVisualStyleBackColor(true);
			((Control)cmdImportOELfile).add_Click((EventHandler)cmdImportOELfile_Click);
			((Control)grpComets).get_Controls().Add((Control)(object)cmdUpdateComets);
			((Control)grpComets).get_Controls().Add((Control)(object)panel1);
			((Control)grpComets).get_Controls().Add((Control)(object)cmbComet);
			((Control)grpComets).get_Controls().Add((Control)(object)cmdImportComet);
			((Control)grpComets).set_Location(new Point(440, 5));
			((Control)grpComets).set_Name("grpComets");
			((Control)grpComets).set_Size(new Size(288, 85));
			((Control)grpComets).set_TabIndex(2);
			grpComets.set_TabStop(false);
			((Control)grpComets).set_Text("Import COMETS");
			((Control)cmdUpdateComets).set_BackColor(Color.PaleTurquoise);
			((Control)cmdUpdateComets).set_Location(new Point(35, 64));
			((Control)cmdUpdateComets).set_Name("cmdUpdateComets");
			((Control)cmdUpdateComets).set_Size(new Size(116, 21));
			((Control)cmdUpdateComets).set_TabIndex(8);
			((Control)cmdUpdateComets).set_Text("Update list of comets");
			((ButtonBase)cmdUpdateComets).set_UseVisualStyleBackColor(false);
			((Control)cmdUpdateComets).add_Click((EventHandler)cmdUpdateComets_Click);
			((Control)panel1).set_BackColor(Color.LemonChiffon);
			((Control)panel1).get_Controls().Add((Control)(object)cmdImportAllCurrentComets);
			((Control)panel1).get_Controls().Add((Control)(object)label22);
			((Control)panel1).get_Controls().Add((Control)(object)optAll);
			((Control)panel1).get_Controls().Add((Control)(object)opt18);
			((Control)panel1).get_Controls().Add((Control)(object)opt14);
			((Control)panel1).set_Location(new Point(186, 7));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(103, 70));
			((Control)panel1).set_TabIndex(7);
			((Control)cmdImportAllCurrentComets).set_Location(new Point(9, 6));
			((Control)cmdImportAllCurrentComets).set_Name("cmdImportAllCurrentComets");
			((Control)cmdImportAllCurrentComets).set_Size(new Size(49, 60));
			((Control)cmdImportAllCurrentComets).set_TabIndex(2);
			((Control)cmdImportAllCurrentComets).set_Text("Import all current comets ");
			((ButtonBase)cmdImportAllCurrentComets).set_UseVisualStyleBackColor(true);
			((Control)cmdImportAllCurrentComets).add_Click((EventHandler)cmdImportAllCurrentComets_Click);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(61, 2));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(31, 13));
			((Control)label22).set_TabIndex(3);
			((Control)label22).set_Text("Mag.");
			((Control)optAll).set_AutoSize(true);
			((Control)optAll).set_Location(new Point(60, 49));
			((Control)optAll).set_Name("optAll");
			((Control)optAll).set_Size(new Size(36, 17));
			((Control)optAll).set_TabIndex(6);
			((Control)optAll).set_Text("All");
			((ButtonBase)optAll).set_UseVisualStyleBackColor(true);
			((Control)opt18).set_AutoSize(true);
			opt18.set_Checked(true);
			((Control)opt18).set_Location(new Point(60, 32));
			((Control)opt18).set_Name("opt18");
			((Control)opt18).set_Size(new Size(43, 17));
			((Control)opt18).set_TabIndex(5);
			opt18.set_TabStop(true);
			((Control)opt18).set_Text("<18");
			((ButtonBase)opt18).set_UseVisualStyleBackColor(true);
			((Control)opt14).set_AutoSize(true);
			((Control)opt14).set_Location(new Point(60, 15));
			((Control)opt14).set_Name("opt14");
			((Control)opt14).set_Size(new Size(43, 17));
			((Control)opt14).set_TabIndex(4);
			((Control)opt14).set_Text("<14");
			((ButtonBase)opt14).set_UseVisualStyleBackColor(true);
			((Control)cmbComet).set_Anchor((AnchorStyles)1);
			cmbComet.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbComet).set_FormattingEnabled(true);
			((Control)cmbComet).set_Location(new Point(6, 41));
			((Control)cmbComet).set_Name("cmbComet");
			((Control)cmbComet).set_Size(new Size(175, 21));
			((Control)cmbComet).set_TabIndex(1);
			((Control)cmdImportComet).set_Location(new Point(6, 14));
			((Control)cmdImportComet).set_Name("cmdImportComet");
			((Control)cmdImportComet).set_Size(new Size(175, 24));
			((Control)cmdImportComet).set_TabIndex(0);
			((Control)cmdImportComet).set_Text("Import the comet selected below");
			((ButtonBase)cmdImportComet).set_UseVisualStyleBackColor(true);
			((Control)cmdImportComet).add_Click((EventHandler)cmdImportComet_Click);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)fileToolStripMenuItem,
				(ToolStripItem)pasteElementsFromJPLHorizonsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1359, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)fileToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)reloadFromFileToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)fileToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)fileToolStripMenuItem).set_Name("fileToolStripMenuItem");
			((ToolStripItem)fileToolStripMenuItem).set_Size(new Size(58, 20));
			((ToolStripItem)fileToolStripMenuItem).set_Text("File...    ");
			((ToolStripItem)reloadFromFileToolStripMenuItem).set_Image((Image)Resources.openfolderHS);
			((ToolStripItem)reloadFromFileToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)reloadFromFileToolStripMenuItem).set_Name("reloadFromFileToolStripMenuItem");
			((ToolStripItem)reloadFromFileToolStripMenuItem).set_Size(new Size(163, 22));
			((ToolStripItem)reloadFromFileToolStripMenuItem).set_Text("Re-load from file");
			((ToolStripItem)reloadFromFileToolStripMenuItem).add_Click((EventHandler)reloadFromFileToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(163, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)pasteElementsFromJPLHorizonsToolStripMenuItem).set_Font(new Font("Segoe UI", 8.5f));
			((ToolStripItem)pasteElementsFromJPLHorizonsToolStripMenuItem).set_Image((Image)Resources.sun_horizon);
			((ToolStripItem)pasteElementsFromJPLHorizonsToolStripMenuItem).set_Name("pasteElementsFromJPLHorizonsToolStripMenuItem");
			((ToolStripItem)pasteElementsFromJPLHorizonsToolStripMenuItem).set_Size(new Size(209, 20));
			((ToolStripItem)pasteElementsFromJPLHorizonsToolStripMenuItem).set_Text("Add elements from JPL-Horizons");
			((ToolStripItem)pasteElementsFromJPLHorizonsToolStripMenuItem).add_Click((EventHandler)PasteElementsFromJPLHorizonsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(75, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help     ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(54, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ScrollableControl)panelData).set_AutoScroll(true);
			((Control)panelData).get_Controls().Add((Control)(object)label1);
			((Control)panelData).get_Controls().Add((Control)(object)label2);
			((Control)panelData).get_Controls().Add((Control)(object)lstElements);
			((Control)panelData).set_Location(new Point(6, 27));
			((Control)panelData).set_Name("panelData");
			((Control)panelData).set_Size(new Size(1326, 328));
			((Control)panelData).set_TabIndex(1);
			((Control)pnlHorizons).set_BackColor(Color.LightGoldenrodYellow);
			pnlHorizons.set_BorderStyle((BorderStyle)2);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)cmdClearElements);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)panel3);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)panelAsteroid);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)panel2);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)cmdHelp);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)label32);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)cmdExit);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)label29);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)label28);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)label27);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)label26);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)txtMonth);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)txtDay);
			((Control)pnlHorizons).get_Controls().Add((Control)(object)txtYear);
			((Control)pnlHorizons).set_Location(new Point(363, 13));
			((Control)pnlHorizons).set_Name("pnlHorizons");
			((Control)pnlHorizons).set_Size(new Size(649, 351));
			((Control)pnlHorizons).set_TabIndex(4);
			((Control)pnlHorizons).set_Visible(false);
			((Control)pnlHorizons).add_MouseDown(new MouseEventHandler(pnlHorizons_MouseDown));
			((Control)pnlHorizons).add_MouseHover((EventHandler)pnlHorizons_MouseHover);
			((Control)pnlHorizons).add_MouseMove(new MouseEventHandler(pnlHorizons_MouseMove));
			((Control)cmdClearElements).set_BackColor(Color.Pink);
			((Control)cmdClearElements).set_Location(new Point(484, 37));
			((Control)cmdClearElements).set_Name("cmdClearElements");
			((Control)cmdClearElements).set_Size(new Size(122, 42));
			((Control)cmdClearElements).set_TabIndex(40);
			((Control)cmdClearElements).set_Text("Delete all elements from the Main list");
			((ButtonBase)cmdClearElements).set_UseVisualStyleBackColor(false);
			((Control)cmdClearElements).add_Click((EventHandler)cmdClearElements_Click);
			((Control)panel3).set_BackColor(Color.SkyBlue);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)cmdClearChecks);
			((Control)panel3).get_Controls().Add((Control)(object)chkLstComets);
			((Control)panel3).get_Controls().Add((Control)(object)cmdCometFromHorizons);
			((Control)panel3).get_Controls().Add((Control)(object)label37);
			((Control)panel3).set_Location(new Point(422, 106));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(219, 236));
			((Control)panel3).set_TabIndex(39);
			((Control)panel3).add_Paint(new PaintEventHandler(panel3_Paint));
			((Control)cmdClearChecks).set_BackColor(Color.LightPink);
			((Control)cmdClearChecks).set_Location(new Point(119, 202));
			((Control)cmdClearChecks).set_Name("cmdClearChecks");
			((Control)cmdClearChecks).set_Size(new Size(91, 27));
			((Control)cmdClearChecks).set_TabIndex(13);
			((Control)cmdClearChecks).set_Text("Clear all checks");
			((ButtonBase)cmdClearChecks).set_UseVisualStyleBackColor(false);
			((Control)cmdClearChecks).add_Click((EventHandler)cmdClearChecks_Click);
			((Control)chkLstComets).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)chkLstComets).set_FormattingEnabled(true);
			((Control)chkLstComets).set_Location(new Point(20, 30));
			((Control)chkLstComets).set_Name("chkLstComets");
			((Control)chkLstComets).set_Size(new Size(175, 169));
			((Control)chkLstComets).set_TabIndex(12);
			chkLstComets.add_ItemCheck(new ItemCheckEventHandler(chkLstComets_ItemCheck));
			((Control)cmdCometFromHorizons).set_BackColor(Color.Yellow);
			((Control)cmdCometFromHorizons).set_Location(new Point(4, 202));
			((Control)cmdCometFromHorizons).set_Name("cmdCometFromHorizons");
			((Control)cmdCometFromHorizons).set_Size(new Size(105, 27));
			((Control)cmdCometFromHorizons).set_TabIndex(11);
			((Control)cmdCometFromHorizons).set_Text("Retrieve elements");
			((ButtonBase)cmdCometFromHorizons).set_UseVisualStyleBackColor(false);
			((Control)cmdCometFromHorizons).add_Click((EventHandler)cmdCometFromHorizons_Click);
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(50, 5));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(115, 17));
			((Control)label37).set_TabIndex(9);
			((Control)label37).set_Text("Select a comet");
			((Control)panelAsteroid).set_BackColor(Color.Aquamarine);
			panelAsteroid.set_BorderStyle((BorderStyle)2);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)label38);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl15);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt15);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl10);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt10);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl5);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt5);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)label33);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl20);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt20);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl30);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt30);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl40);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt40);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl400);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl200);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl100);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lbl50);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt100);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt200);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt400);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)optNumber);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)opt50);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)lblCurrentNum);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)cmdAbort);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)cmdHorizons);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)label31);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)txtAsteroids);
			((Control)panelAsteroid).get_Controls().Add((Control)(object)label30);
			((Control)panelAsteroid).set_Location(new Point(2, 106));
			((Control)panelAsteroid).set_Name("panelAsteroid");
			((Control)panelAsteroid).set_Size(new Size(416, 236));
			((Control)panelAsteroid).set_TabIndex(38);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_ForeColor(Color.DarkBlue);
			((Control)label33).set_Location(new Point(263, 104));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(137, 13));
			((Control)label33).set_TabIndex(31);
			((Control)label33).set_Text("Options for 'by Number' are:");
			((Control)lbl20).set_AutoSize(true);
			((Control)lbl20).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl20).set_Location(new Point(275, 76));
			((Control)lbl20).set_Name("lbl20");
			((Control)lbl20).set_Size(new Size(13, 13));
			((Control)lbl20).set_TabIndex(30);
			((Control)lbl20).set_Text("0");
			((Control)opt20).set_AutoSize(true);
			opt20.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt20).set_Location(new Point(259, 43));
			((Control)opt20).set_Name("opt20");
			((Control)opt20).set_Size(new Size(43, 30));
			((Control)opt20).set_TabIndex(29);
			((Control)opt20).set_Text(">20km");
			((ButtonBase)opt20).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt20).set_UseVisualStyleBackColor(true);
			((Control)lbl30).set_AutoSize(true);
			((Control)lbl30).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl30).set_Location(new Point(236, 76));
			((Control)lbl30).set_Name("lbl30");
			((Control)lbl30).set_Size(new Size(13, 13));
			((Control)lbl30).set_TabIndex(28);
			((Control)lbl30).set_Text("0");
			((Control)opt30).set_AutoSize(true);
			opt30.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt30).set_Location(new Point(220, 43));
			((Control)opt30).set_Name("opt30");
			((Control)opt30).set_Size(new Size(43, 30));
			((Control)opt30).set_TabIndex(27);
			((Control)opt30).set_Text(">30km");
			((ButtonBase)opt30).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt30).set_UseVisualStyleBackColor(true);
			((Control)lbl40).set_AutoSize(true);
			((Control)lbl40).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl40).set_Location(new Point(196, 76));
			((Control)lbl40).set_Name("lbl40");
			((Control)lbl40).set_Size(new Size(13, 13));
			((Control)lbl40).set_TabIndex(26);
			((Control)lbl40).set_Text("0");
			((Control)opt40).set_AutoSize(true);
			opt40.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt40).set_Location(new Point(180, 43));
			((Control)opt40).set_Name("opt40");
			((Control)opt40).set_Size(new Size(43, 30));
			((Control)opt40).set_TabIndex(25);
			((Control)opt40).set_Text(">40km");
			((ButtonBase)opt40).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt40).set_UseVisualStyleBackColor(true);
			((Control)lbl400).set_AutoSize(true);
			((Control)lbl400).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl400).set_Location(new Point(16, 76));
			((Control)lbl400).set_Name("lbl400");
			((Control)lbl400).set_Size(new Size(13, 13));
			((Control)lbl400).set_TabIndex(14);
			((Control)lbl400).set_Text("0");
			((Control)lbl200).set_AutoSize(true);
			((Control)lbl200).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl200).set_Location(new Point(64, 76));
			((Control)lbl200).set_Name("lbl200");
			((Control)lbl200).set_Size(new Size(13, 13));
			((Control)lbl200).set_TabIndex(15);
			((Control)lbl200).set_Text("0");
			((Control)lbl100).set_AutoSize(true);
			((Control)lbl100).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl100).set_Location(new Point(112, 76));
			((Control)lbl100).set_Name("lbl100");
			((Control)lbl100).set_Size(new Size(13, 13));
			((Control)lbl100).set_TabIndex(16);
			((Control)lbl100).set_Text("0");
			((Control)lbl50).set_AutoSize(true);
			((Control)lbl50).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl50).set_Location(new Point(156, 76));
			((Control)lbl50).set_Name("lbl50");
			((Control)lbl50).set_Size(new Size(13, 13));
			((Control)lbl50).set_TabIndex(17);
			((Control)lbl50).set_Text("0");
			((Control)opt100).set_AutoSize(true);
			opt100.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt100).set_Location(new Point(93, 43));
			((Control)opt100).set_Name("opt100");
			((Control)opt100).set_Size(new Size(49, 30));
			((Control)opt100).set_TabIndex(12);
			((Control)opt100).set_Text(">100km");
			((ButtonBase)opt100).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt100).set_UseVisualStyleBackColor(true);
			opt100.add_CheckedChanged((EventHandler)opt100_CheckedChanged);
			((Control)opt200).set_AutoSize(true);
			opt200.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt200).set_Location(new Point(46, 43));
			((Control)opt200).set_Name("opt200");
			((Control)opt200).set_Size(new Size(49, 30));
			((Control)opt200).set_TabIndex(11);
			((Control)opt200).set_Text(">200km");
			((ButtonBase)opt200).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt200).set_UseVisualStyleBackColor(true);
			opt200.add_CheckedChanged((EventHandler)opt200_CheckedChanged);
			((Control)opt400).set_AutoSize(true);
			opt400.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt400).set_Location(new Point(-1, 43));
			((Control)opt400).set_Name("opt400");
			((Control)opt400).set_Size(new Size(49, 30));
			((Control)opt400).set_TabIndex(10);
			((Control)opt400).set_Text(">400km");
			((ButtonBase)opt400).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt400).set_UseVisualStyleBackColor(true);
			opt400.add_CheckedChanged((EventHandler)opt400_CheckedChanged);
			((Control)optNumber).set_AutoSize(true);
			optNumber.set_CheckAlign(ContentAlignment.BottomCenter);
			optNumber.set_Checked(true);
			((Control)optNumber).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optNumber).set_ForeColor(Color.MediumBlue);
			((Control)optNumber).set_Location(new Point(6, 121));
			((Control)optNumber).set_Name("optNumber");
			((Control)optNumber).set_Size(new Size(71, 30));
			((Control)optNumber).set_TabIndex(9);
			optNumber.set_TabStop(true);
			((Control)optNumber).set_Text("by Number");
			((ButtonBase)optNumber).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)optNumber).set_UseVisualStyleBackColor(true);
			optNumber.add_CheckedChanged((EventHandler)optNumber_CheckedChanged);
			((Control)opt50).set_AutoSize(true);
			opt50.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt50).set_Location(new Point(140, 43));
			((Control)opt50).set_Name("opt50");
			((Control)opt50).set_Size(new Size(43, 30));
			((Control)opt50).set_TabIndex(13);
			((Control)opt50).set_Text(">50km");
			((ButtonBase)opt50).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt50).set_UseVisualStyleBackColor(true);
			opt50.add_CheckedChanged((EventHandler)opt50_CheckedChanged);
			((Control)lblCurrentNum).set_AutoSize(true);
			((Control)lblCurrentNum).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblCurrentNum).set_Location(new Point(18, 210));
			((Control)lblCurrentNum).set_Name("lblCurrentNum");
			((Control)lblCurrentNum).set_Size(new Size(16, 17));
			((Control)lblCurrentNum).set_TabIndex(20);
			((Control)lblCurrentNum).set_Text("0");
			((Control)lblCurrentNum).set_Visible(false);
			((Control)cmdAbort).set_Location(new Point(304, 200));
			((Control)cmdAbort).set_Name("cmdAbort");
			((Control)cmdAbort).set_Size(new Size(53, 27));
			((Control)cmdAbort).set_TabIndex(22);
			((Control)cmdAbort).set_Text("Cancel");
			((ButtonBase)cmdAbort).set_UseVisualStyleBackColor(true);
			((Control)cmdAbort).set_Visible(false);
			((Control)cmdAbort).add_Click((EventHandler)cmdAbort_Click);
			((Control)cmdHorizons).set_BackColor(Color.Yellow);
			((Control)cmdHorizons).set_Location(new Point(142, 182));
			((Control)cmdHorizons).set_Name("cmdHorizons");
			((Control)cmdHorizons).set_Size(new Size(125, 45));
			((Control)cmdHorizons).set_TabIndex(21);
			((Control)cmdHorizons).set_Text("Retrieve elements");
			((ButtonBase)cmdHorizons).set_UseVisualStyleBackColor(false);
			((Control)cmdHorizons).add_Click((EventHandler)cmdHorizons_Click);
			((Control)label31).set_AutoSize(true);
			label31.set_BorderStyle((BorderStyle)1);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(257, 118));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(146, 47));
			((Control)label31).set_TabIndex(18);
			((Control)label31).set_Text("* single number\r\n* a range  x - y  : or   - y\r\n* multiple asteroids  a,b,c");
			((Control)txtAsteroids).set_Location(new Point(79, 131));
			((Control)txtAsteroids).set_Name("txtAsteroids");
			((Control)txtAsteroids).set_Size(new Size(169, 20));
			((Control)txtAsteroids).set_TabIndex(19);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(115, 0));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(161, 17));
			((Control)label30).set_TabIndex(8);
			((Control)label30).set_Text("Specify the asteroids");
			((Control)panel2).set_BackColor(Color.Khaki);
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)label36);
			((Control)panel2).get_Controls().Add((Control)(object)label35);
			((Control)panel2).get_Controls().Add((Control)(object)txtMinute);
			((Control)panel2).get_Controls().Add((Control)(object)label34);
			((Control)panel2).get_Controls().Add((Control)(object)txtHour);
			((Control)panel2).set_Location(new Point(210, 56));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(197, 48));
			((Control)panel2).set_TabIndex(37);
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(8, 4));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(110, 36));
			((Control)label36).set_TabIndex(36);
			((Control)label36).set_Text("Set hour =0 and Min =0\r\nOnly set different times\r\nif asteroid very near Earth");
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Location(new Point(157, 1));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(24, 13));
			((Control)label35).set_TabIndex(34);
			((Control)label35).set_Text("Min");
			((Control)txtMinute).set_Location(new Point(157, 15));
			((Control)txtMinute).set_Name("txtMinute");
			((Control)txtMinute).set_Size(new Size(30, 20));
			((Control)txtMinute).set_TabIndex(35);
			((Control)txtMinute).set_Text("0");
			txtMinute.set_TextAlign((HorizontalAlignment)2);
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Location(new Point(120, 1));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(30, 13));
			((Control)label34).set_TabIndex(32);
			((Control)label34).set_Text("Hour");
			((Control)txtHour).set_Location(new Point(120, 15));
			((Control)txtHour).set_Name("txtHour");
			((Control)txtHour).set_Size(new Size(30, 20));
			((Control)txtHour).set_TabIndex(33);
			((Control)txtHour).set_Text("0");
			txtHour.set_TextAlign((HorizontalAlignment)2);
			((Control)txtHour).add_KeyPress(new KeyPressEventHandler(txtHour_KeyPress));
			((Control)cmdHelp).set_AutoSize(true);
			((ButtonBase)cmdHelp).set_Image((Image)Resources.help);
			((Control)cmdHelp).set_Location(new Point(0, 0));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(38, 38));
			((Control)cmdHelp).set_TabIndex(24);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(true);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 14f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(188, 5));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(269, 24));
			((Control)label32).set_TabIndex(0);
			((Control)label32).set_Text("Get elements from Horizons");
			((ButtonBase)cmdExit).set_Image((Image)Resources.error);
			((Control)cmdExit).set_Location(new Point(616, 3));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(21, 24));
			((Control)cmdExit).set_TabIndex(23);
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(true);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label29).set_ForeColor(Color.BlueViolet);
			((Control)label29).set_Location(new Point(112, 35));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(232, 17));
			((Control)label29).set_TabIndex(1);
			((Control)label29).set_Text("Set the epoch for the elements");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Location(new Point(168, 56));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(26, 13));
			((Control)label28).set_TabIndex(6);
			((Control)label28).set_Text("Day");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Location(new Point(125, 56));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(37, 13));
			((Control)label27).set_TabIndex(4);
			((Control)label27).set_Text("Month");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Location(new Point(84, 56));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(29, 13));
			((Control)label26).set_TabIndex(2);
			((Control)label26).set_Text("Year");
			((Control)txtMonth).set_Location(new Point(128, 70));
			((Control)txtMonth).set_Name("txtMonth");
			((Control)txtMonth).set_Size(new Size(30, 20));
			((Control)txtMonth).set_TabIndex(5);
			((Control)txtMonth).add_KeyPress(new KeyPressEventHandler(txtMonth_KeyPress));
			((Control)txtDay).set_Location(new Point(166, 70));
			((Control)txtDay).set_Name("txtDay");
			((Control)txtDay).set_Size(new Size(30, 20));
			((Control)txtDay).set_TabIndex(7);
			((Control)txtDay).add_KeyPress(new KeyPressEventHandler(txtDay_KeyPress));
			((Control)txtYear).set_Location(new Point(77, 70));
			((Control)txtYear).set_Name("txtYear");
			((Control)txtYear).set_Size(new Size(43, 20));
			((Control)txtYear).set_TabIndex(3);
			((Control)txtYear).add_KeyPress(new KeyPressEventHandler(txtYear_KeyPress));
			((Control)cmbCometHorizons).set_Anchor((AnchorStyles)1);
			cmbCometHorizons.set_DropDownStyle((ComboBoxStyle)2);
			((ListControl)cmbCometHorizons).set_FormattingEnabled(true);
			((Control)cmbCometHorizons).set_Location(new Point(1069, 390));
			((Control)cmbCometHorizons).set_Name("cmbCometHorizons");
			((Control)cmbCometHorizons).set_Size(new Size(175, 21));
			((Control)cmbCometHorizons).set_TabIndex(10);
			((Control)lbl5).set_AutoSize(true);
			((Control)lbl5).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl5).set_Location(new Point(387, 76));
			((Control)lbl5).set_Name("lbl5");
			((Control)lbl5).set_Size(new Size(13, 13));
			((Control)lbl5).set_TabIndex(33);
			((Control)lbl5).set_Text("0");
			((Control)opt5).set_AutoSize(true);
			opt5.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt5).set_Location(new Point(374, 43));
			((Control)opt5).set_Name("opt5");
			((Control)opt5).set_Size(new Size(37, 30));
			((Control)opt5).set_TabIndex(32);
			((Control)opt5).set_Text(">5km");
			((ButtonBase)opt5).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt5).set_UseVisualStyleBackColor(true);
			((Control)lbl10).set_AutoSize(true);
			((Control)lbl10).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl10).set_Location(new Point(350, 76));
			((Control)lbl10).set_Name("lbl10");
			((Control)lbl10).set_Size(new Size(13, 13));
			((Control)lbl10).set_TabIndex(35);
			((Control)lbl10).set_Text("0");
			((Control)opt10).set_AutoSize(true);
			opt10.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt10).set_Location(new Point(335, 43));
			((Control)opt10).set_Name("opt10");
			((Control)opt10).set_Size(new Size(43, 30));
			((Control)opt10).set_TabIndex(34);
			((Control)opt10).set_Text(">10km");
			((ButtonBase)opt10).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt10).set_UseVisualStyleBackColor(true);
			((Control)lbl15).set_AutoSize(true);
			((Control)lbl15).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lbl15).set_Location(new Point(313, 76));
			((Control)lbl15).set_Name("lbl15");
			((Control)lbl15).set_Size(new Size(13, 13));
			((Control)lbl15).set_TabIndex(37);
			((Control)lbl15).set_Text("0");
			((Control)opt15).set_AutoSize(true);
			opt15.set_CheckAlign(ContentAlignment.BottomCenter);
			((Control)opt15).set_Location(new Point(297, 43));
			((Control)opt15).set_Name("opt15");
			((Control)opt15).set_Size(new Size(43, 30));
			((Control)opt15).set_TabIndex(36);
			((Control)opt15).set_Text(">15km");
			((ButtonBase)opt15).set_TextAlign(ContentAlignment.TopCenter);
			((ButtonBase)opt15).set_UseVisualStyleBackColor(true);
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label38).set_ForeColor(Color.MediumBlue);
			((Control)label38).set_Location(new Point(6, 27));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(53, 13));
			((Control)label38).set_TabIndex(38);
			((Control)label38).set_Text("By SIZE");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1359, 580));
			((Control)this).get_Controls().Add((Control)(object)grpEdit);
			((Control)this).get_Controls().Add((Control)(object)pnlHorizons);
			((Control)this).get_Controls().Add((Control)(object)panelData);
			((Control)this).get_Controls().Add((Control)(object)cmbCometHorizons);
			((Control)this).get_Controls().Add((Control)(object)panelMain);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationUserAsteroids", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationUserAsteroids);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("UserAsteroids_Edit");
			((Control)this).set_Text("Edit file of user minor planets");
			((Form)this).add_FormClosing(new FormClosingEventHandler(UserAsteroids_Edit_FormClosing));
			((Form)this).add_FormClosed(new FormClosedEventHandler(UserAsteroids_Edit_FormClosed));
			((Form)this).add_Load((EventHandler)UserAsteroids_Edit_Load);
			((Control)this).add_Resize((EventHandler)UserAsteroids_Edit_Resize);
			((Control)groupOppMag).ResumeLayout(false);
			((Control)groupOppMag).PerformLayout();
			((ISupportInitialize)updnMag).EndInit();
			((ISupportInitialize)updnYear).EndInit();
			((ISupportInitialize)updnMonth).EndInit();
			((ISupportInitialize)updnDay).EndInit();
			((Control)grpEdit).ResumeLayout(false);
			((Control)grpEdit).PerformLayout();
			((Control)panelMain).ResumeLayout(false);
			((Control)groupBox1).ResumeLayout(false);
			((Control)grpComets).ResumeLayout(false);
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)panelData).ResumeLayout(false);
			((Control)panelData).PerformLayout();
			((Control)pnlHorizons).ResumeLayout(false);
			((Control)pnlHorizons).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((Control)panelAsteroid).ResumeLayout(false);
			((Control)panelAsteroid).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
