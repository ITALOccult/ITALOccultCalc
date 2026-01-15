using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class Star_DisplayRange : Form
	{
		private readonly string AppPath;

		private int Counter;

		private string[] ReservedFilesLower;

		private string Columns;

		private string Units;

		private IContainer components;

		private ListBox lstStars;

		private GroupBox grpNomad;

		private ComboBox cmbNomadDec;

		private ComboBox cmbNomadRA;

		private Button cmdNomad;

		private GroupBox grpTycho2;

		private Button cmdTycho2;

		private ComboBox cmbTycho2_Dec;

		private ComboBox cmbTycho2_RA;

		private GroupBox grpUser;

		private Button cmdUser;

		private ComboBox cmbUser_Dec;

		private ComboBox cmbUser_RA;

		private GroupBox grpXZ80Q;

		private Button cmdXZ80Q;

		private ComboBox cmbXZ80_RA;

		private MenuStrip menuStrip1;

		private Label label4;

		private Label label1;

		private Label label2;

		private Label label5;

		private ToolStripMenuItem withListOfStarsToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printPreviewToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		private ComboBox cmbUserCat;

		private Button GoogleSky_Tycho2;

		private Button GoogleSky_NOMAD;

		private Button GoogleSky_XZ80Q;

		private Button GoogleSky_User;

		private ToolTip toolTip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem withListToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem1;

		private GroupBox grpUCAC4;

		private Button GoogleSky_UCAC4;

		private Label label7;

		private Button cmdUCAC4;

		private ComboBox cmbUCAC4_Dec;

		private ComboBox cmbUCAC4_RA;

		private GroupBox grpPPMXL;

		private Button GoogleSky_PPMXL;

		private Label label8;

		private Button cmdPPMXL;

		private ComboBox cmbPPMXL_Dec;

		private ComboBox cmbPPMXL_RA;

		private CheckBox chkHip;

		private Panel panelCats;

		private ContextMenuStrip contextMenuStrip1;

		private ToolStripMenuItem copyStarEntryToolStripMenuItem;

		private ToolStripMenuItem catalogueComparisonVizieRToolStripMenuItem;

		private ToolStripMenuItem displayInGoogleSkyToolStripMenuItem;

		private Label label10;

		private GroupBox grpGaia;

		private Button GoogleSky_TychoGaia;

		private Label label11;

		private Button cmdGaia;

		private ComboBox cmbGaia_Dec;

		private ComboBox cmbGaia_RA;

		private Label lblGaiaCat;

		private ListBox lstGaiaFiles;

		private Label lblColumns;

		private Label lblUnits;

		public Star_DisplayRange()
		{
			//IL_0077: Unknown result type (might be due to invalid IL or missing references)
			//IL_0081: Expected O, but got Unknown
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			InitializeComponent();
			AppPath = Utilities.AppPath;
			ReservedFilesLower = new string[Gaia.ReservedGaiaFiles.Length];
			for (int i = 0; i < Gaia.ReservedGaiaFiles.Length; i++)
			{
				ReservedFilesLower[i] = Gaia.ReservedGaiaFiles[i].ToLower();
			}
			lstGaiaFiles.set_DrawMode((DrawMode)1);
			lstGaiaFiles.add_DrawItem(new DrawItemEventHandler(lstGaiaFiles_DrawItem));
			((Control)lblColumns).set_Text("     ID             Mb    Mv    Mr    Right Ascension   pm RA   e(RA) e(pm)  epoch     Declination     pm Dec e(Dec) e(pm)  epoch     RV  Parx e(Parx)  Dia   Rely  d n u p  Gaia Source ID");
			((Control)lblUnits).set_Text("                                       h  m   s          sec      mas   mas  epoch       o  '   \"        asec    mas   mas  epoch    km/s  mas   mas    mas   Rely  d n u p  Gaia Source ID");
		}

		private void lstGaiaFiles_DrawItem(object sender, DrawItemEventArgs e)
		{
			Color color = Color.FromArgb(255, 128, 255, 0);
			Color color2 = Color.FromArgb(255, 255, 238, 225);
			if (e.get_Index() <= -1)
			{
				return;
			}
			if (e.get_Index() == ((ListControl)lstGaiaFiles).get_SelectedIndex())
			{
				e.get_Graphics().FillRectangle(Brushes.Blue, e.get_Bounds());
			}
			else if (e.get_Index() < 2)
			{
				e.get_Graphics().FillRectangle(Brushes.Firebrick, e.get_Bounds());
			}
			else if (e.get_Index() < Gaia.GaiaPrimaryFiles.Count + 3)
			{
				e.get_Graphics().FillRectangle(new SolidBrush(color), e.get_Bounds());
			}
			else if (e.get_Index() < Gaia.GaiaAdditionalFiles.Count + Gaia.GaiaPrimaryFiles.Count + 3)
			{
				e.get_Graphics().FillRectangle(new SolidBrush(color2), e.get_Bounds());
			}
			else
			{
				e.DrawBackground();
			}
			using Brush brush = new SolidBrush(e.get_ForeColor());
			if (e.get_Index() < 2)
			{
				e.get_Graphics().DrawString(lstGaiaFiles.get_Items().get_Item(e.get_Index()).ToString(), e.get_Font(), Brushes.Yellow, e.get_Bounds().Location);
			}
			else
			{
				e.get_Graphics().DrawString(lstGaiaFiles.get_Items().get_Item(e.get_Index()).ToString(), e.get_Font(), brush, e.get_Bounds().Location);
			}
		}

		private void Star_DisplayRange_Load(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)panelCats).set_Width(((Control)this).get_Width() - 19);
			((Control)lstStars).set_Width(((Control)this).get_Width() - 19);
			((Control)lstStars).set_Height(((Control)this).get_Height() - 179);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			cmbXZ80_RA.get_Items().Clear();
			cmbTycho2_RA.get_Items().Clear();
			cmbUser_RA.get_Items().Clear();
			cmbTycho2_Dec.get_Items().Clear();
			cmbUser_Dec.get_Items().Clear();
			for (int i = 0; i < 360; i++)
			{
				cmbXZ80_RA.get_Items().Add((object)Utilities.DEGtoDMS((double)i / 15.0, 2, 1, MinutesOnly: true));
				cmbGaia_RA.get_Items().Add((object)Utilities.DEGtoDMS((double)i / 15.0, 2, 1, MinutesOnly: true));
				cmbTycho2_RA.get_Items().Add((object)Utilities.DEGtoDMS((double)i / 15.0, 2, 1, MinutesOnly: true));
				cmbUser_RA.get_Items().Add((object)Utilities.DEGtoDMS((double)i / 15.0, 2, 1, MinutesOnly: true));
			}
			for (int j = 0; j < 180; j++)
			{
				cmbTycho2_Dec.get_Items().Add((object)Utilities.DEGtoDMS(-90 + j, 3, 0, MinutesOnly: true));
				cmbUser_Dec.get_Items().Add((object)Utilities.DEGtoDMS(-90 + j, 3, 0, MinutesOnly: true));
			}
			ComboBox obj = cmbXZ80_RA;
			ComboBox obj2 = cmbTycho2_RA;
			ComboBox obj3 = cmbUser_RA;
			ComboBox obj4 = cmbTycho2_Dec;
			int num;
			((ListControl)cmbUser_Dec).set_SelectedIndex(num = 0);
			int num2;
			((ListControl)obj4).set_SelectedIndex(num2 = num);
			int num3;
			((ListControl)obj3).set_SelectedIndex(num3 = num2);
			int selectedIndex;
			((ListControl)obj2).set_SelectedIndex(selectedIndex = num3);
			((ListControl)obj).set_SelectedIndex(selectedIndex);
			cmbUCAC4_RA.get_Items().Clear();
			cmbUCAC4_Dec.get_Items().Clear();
			cmbPPMXL_RA.get_Items().Clear();
			cmbPPMXL_Dec.get_Items().Clear();
			cmbNomadRA.get_Items().Clear();
			cmbNomadDec.get_Items().Clear();
			for (int k = 0; k < 1440; k++)
			{
				cmbUCAC4_RA.get_Items().Add((object)Utilities.DEGtoDMS((double)k / 60.0, 2, 1, MinutesOnly: true));
			}
			for (int l = 0; l < 1800; l++)
			{
				cmbPPMXL_RA.get_Items().Add((object)Utilities.DEGtoDMS((double)l / 75.0, 2, 1, MinutesOnly: true));
			}
			for (int m = 0; m < 360; m++)
			{
				cmbGaia_Dec.get_Items().Add((object)Utilities.DEGtoDMS(-90.0 + (double)m / 2.0, 3, 0, MinutesOnly: true));
			}
			for (int n = 0; n < 900; n++)
			{
				cmbUCAC4_Dec.get_Items().Add((object)Utilities.DEGtoDMS(-90.0 + (double)n / 5.0, 3, 0, MinutesOnly: true));
			}
			for (int num4 = 0; num4 < 720; num4++)
			{
				cmbPPMXL_Dec.get_Items().Add((object)Utilities.DEGtoDMS(-90.0 + (double)num4 / 4.0, 3, 0, MinutesOnly: true));
			}
			ComboBox obj5 = cmbGaia_RA;
			((ListControl)cmbGaia_Dec).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj5).set_SelectedIndex(selectedIndex);
			ComboBox obj6 = cmbUCAC4_RA;
			((ListControl)cmbUCAC4_Dec).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj6).set_SelectedIndex(selectedIndex);
			ComboBox obj7 = cmbPPMXL_RA;
			((ListControl)cmbPPMXL_Dec).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj7).set_SelectedIndex(selectedIndex);
			for (int num5 = 0; num5 < 1800; num5++)
			{
				cmbNomadRA.get_Items().Add((object)Utilities.DEGtoDMS((double)num5 / 75.0, 2, 1, MinutesOnly: true));
				cmbNomadDec.get_Items().Add((object)Utilities.DEGtoDMS(-90.0 + (double)num5 / 10.0, 3, 0, MinutesOnly: true));
			}
			ComboBox obj8 = cmbNomadRA;
			((ListControl)cmbNomadDec).set_SelectedIndex(selectedIndex = 0);
			((ListControl)obj8).set_SelectedIndex(selectedIndex);
			((Control)grpXZ80Q).set_Enabled(File.Exists(AppPath + "\\Resource Files\\XZ80.dat"));
			lstGaiaFiles.get_Items().Clear();
			lstGaiaFiles.get_Items().Add((object)"Double-click to select");
			lstGaiaFiles.get_Items().Add((object)"______________________");
			Gaia.GetAvailableGaiaCatalogues();
			for (int num6 = 0; num6 < Gaia.GaiaPrimaryFiles.Count; num6++)
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.GaiaPrimaryFiles[num6]);
			}
			lstGaiaFiles.get_Items().Add((object)"______________________");
			for (int num7 = 0; num7 < Gaia.GaiaAdditionalFiles.Count; num7++)
			{
				lstGaiaFiles.get_Items().Add((object)Gaia.GaiaAdditionalFiles[num7]);
			}
			if (lstGaiaFiles.get_Items().get_Count() > 0)
			{
				((ListControl)lstGaiaFiles).set_SelectedIndex(0);
			}
			else
			{
				((Control)lblGaiaCat).set_Text("no Gaia files");
				((Control)cmdGaia).set_Enabled(false);
			}
			((Control)grpTycho2).set_Enabled(File.Exists(AppPath + "\\Resource Files\\Tycho2.bin"));
			((Control)grpUCAC4).set_Enabled(File.Exists(Settings.Default.UCAC4_Path + "\\u4b\\z001"));
			((Control)grpPPMXL).set_Enabled(File.Exists(Settings.Default.PPMXL_Path + "\\s89d.dat"));
			string text = Settings.Default.NOMAD_Path;
			if (!text.ToLower().Contains("nomad") | !File.Exists(text + "\\090\\m0900.cat"))
			{
				text = Settings.Default.NOMAD_Short_path;
			}
			((Control)grpNomad).set_Enabled(File.Exists(text + "\\090\\m0900.cat") & File.Exists(text + "\\090\\m0900.inx"));
			if (File.Exists(AppPath + "\\Resource Files\\UserA.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserA");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserB.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserB");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserC.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserC");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserD.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserD");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserE.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserE");
			}
			if (File.Exists(AppPath + "\\Resource Files\\UserF.bin"))
			{
				cmbUserCat.get_Items().Add((object)"UserF");
			}
			if (cmbUserCat.get_Items().get_Count() > 0)
			{
				((ListControl)cmbUserCat).set_SelectedIndex(0);
			}
			((Control)grpUser).set_Enabled(cmbUserCat.get_Items().get_Count() > 0);
			Button googleSky_NOMAD = GoogleSky_NOMAD;
			Button googleSky_Tycho = GoogleSky_Tycho2;
			Button googleSky_Tycho2 = GoogleSky_Tycho2;
			Button googleSky_User = GoogleSky_User;
			bool googleEarthInstalled;
			((Control)GoogleSky_XZ80Q).set_Enabled(googleEarthInstalled = Settings.Default.GoogleEarthInstalled);
			bool flag;
			((Control)googleSky_User).set_Enabled(flag = googleEarthInstalled);
			bool flag2;
			((Control)googleSky_Tycho2).set_Enabled(flag2 = flag);
			bool enabled;
			((Control)googleSky_Tycho).set_Enabled(enabled = flag2);
			((Control)googleSky_NOMAD).set_Enabled(enabled);
			Cursor.set_Current(Cursors.get_Default());
		}

		private void cmdNomad_Click(object sender, EventArgs e)
		{
			if (!NOMAD.InitialiseNOMAD())
			{
				return;
			}
			lstStars.get_Items().Clear();
			string text = "Nomad ID         Mv    Mb    Mr    Right Ascension  pm(RA)     e(RA)  e(PM)   epoch      Declination    pm(Dec)  e(Dec)  e(PM)   epoch   Parallax  2UCAC        Hip/Tycho2     USNO-B1      Error";
			string text2 = "                                      h  m   s       sec        \"      \"                  o  '   \"       \"         \"      \"               \"                                                 code";
			((Control)lblColumns).set_Text(text);
			lstStars.get_Items().Add((object)text);
			((Control)lblUnits).set_Text(text2);
			lstStars.get_Items().Add((object)text2);
			int selectedIndex = ((ListControl)cmbNomadRA).get_SelectedIndex();
			NOMAD.Open_Nomad_Catalogue_and_Index_Files(((ListControl)cmbNomadDec).get_SelectedIndex());
			int nomadIndexValue = NOMAD.GetNomadIndexValue(selectedIndex);
			int nomadIndexValue2 = NOMAD.GetNomadIndexValue(selectedIndex + 1);
			Counter = 0;
			for (int i = nomadIndexValue; i <= nomadIndexValue2; i++)
			{
				NOMAD.Read_NOMAD_entry(i);
				lstStars.get_Items().Add((object)NOMAD.Nomad_ASCII_line);
				Counter++;
				if (Counter % 5 == 0)
				{
					lstStars.get_Items().Add((object)"");
				}
			}
			NOMAD.Close_Nomad_Catalogue_and_Index_Files();
			NOMAD.ReleaseNOMAD();
		}

		private void cmdXZ80Q_Click(object sender, EventArgs e)
		{
			lstStars.get_Items().Clear();
			FileStream fileStream = new FileStream(AppPath + "\\Resource Files\\XZ80.inx", FileMode.Open, FileAccess.Read);
			StreamReader streamReader = new StreamReader(fileStream);
			fileStream.Seek(9 * ((ListControl)cmbXZ80_RA).get_SelectedIndex(), SeekOrigin.Begin);
			int num = int.Parse(streamReader.ReadLine());
			int num2 = int.Parse(streamReader.ReadLine());
			streamReader.Close();
			FileStream fileStream2 = new FileStream(AppPath + "\\Resource Files\\XZ80.dat", FileMode.Open, FileAccess.Read);
			BinaryReader binaryReader = new BinaryReader(fileStream2);
			string text = " XZ80Q    Mv    Mb    Mr  vd Sp  Right Ascension  pm(RA)    Declination     pm(Dec)      ZC      SAO";
			string text2 = "                          vd Sp    h  m   s        sec        o  '   \"       \" ";
			((Control)lblColumns).set_Text(text);
			lstStars.get_Items().Add((object)text);
			((Control)lblUnits).set_Text(text2);
			lstStars.get_Items().Add((object)text2);
			Counter = 0;
			for (int i = num; i < num2; i++)
			{
				XZ80Q.ReadStarEntry(fileStream2, binaryReader, i - 1);
				lstStars.get_Items().Add((object)XZ80Q.ToString());
				Counter++;
				if (Counter % 5 == 0)
				{
					lstStars.get_Items().Add((object)"");
				}
			}
			binaryReader.Close();
		}

		private void cmdGaia_Click(object sender, EventArgs e)
		{
			//IL_00eb: Unknown result type (might be due to invalid IL or missing references)
			Gaia.CurrentRecordLength = 48L;
			if (((Control)lblGaiaCat).get_Text().Contains("DR3"))
			{
				Gaia.CurrentRecordLength = 58L;
			}
			lstStars.get_Items().Clear();
			string text = "     ID               Mv    Mb    Mr    Right Ascension   pm RA   e(RA) e(pm)  epoch     Declination     pm Dec e(Dec) e(pm)  epoch     RV    Parx e(Parx)  Dia   Rely  d n u p  Gaia Source ID";
			string text2 = "                                         h  m   s          sec      mas   mas  epoch      o  '   \"         asec    mas   mas  epoch    km/s    mas   mas    mas   Rely  d n u p  Gaia Source ID";
			if (Gaia.CurrentRecordLength == 48)
			{
				text = "     ID               Mv    Mr    Right Ascension   pm RA   e(RA) e(pm)  epoch     Declination     pm Dec e(Dec) e(pm)  epoch     RV    Parx e(Parx)";
				text2 = "                                   h  m   s          sec      mas   mas  epoch        o  '   \"         asec    mas   mas  epoch    km/s    mas   mas";
			}
			((Control)lblColumns).set_Text(text);
			lstStars.get_Items().Add((object)text);
			((Control)lblUnits).set_Text(text2);
			lstStars.get_Items().Add((object)text2);
			int num = 0;
			int selectedIndex = ((ListControl)cmbGaia_RA).get_SelectedIndex();
			int num2 = 359 - ((ListControl)cmbGaia_Dec).get_SelectedIndex();
			if (!File.Exists(Utilities.AppPath + "\\Resource files\\Gaia\\" + ((Control)lblGaiaCat).get_Text() + ".inx"))
			{
				MessageBox.Show("You must select a Gaia catalogue", "No catalogue selected", (MessageBoxButtons)0, (MessageBoxIcon)48);
				return;
			}
			int num4;
			int num5;
			using (FileStream fileStream = new FileStream(Utilities.AppPath + "\\Resource files\\Gaia\\" + ((Control)lblGaiaCat).get_Text() + ".inx", FileMode.Open, FileAccess.Read))
			{
				using BinaryReader binaryReader = new BinaryReader(fileStream);
				int num3 = 361 * num2 + selectedIndex;
				fileStream.Seek(4 * num3, SeekOrigin.Begin);
				num4 = binaryReader.ReadInt32();
				num5 = binaryReader.ReadInt32() - 1;
				if (num5 - num4 < 10 && selectedIndex < 359)
				{
					num5 = num4 + 90;
				}
				num3 = 361 * num2 + 360;
				fileStream.Seek(4 * num3, SeekOrigin.Begin);
				num = binaryReader.ReadInt32() - 1;
				if (num5 > num)
				{
					num5 = num;
				}
			}
			double num6 = (double)(180 - num2) / 2.0;
			double num7 = num6 - 0.5;
			using FileStream fileStream2 = new FileStream(Utilities.AppPath + "\\Resource files\\Gaia\\" + ((Control)lblGaiaCat).get_Text() + ".bin", FileMode.Open, FileAccess.Read);
			using BinaryReader gaia = new BinaryReader(fileStream2);
			Counter = 0;
			for (int i = num4; i <= num5; i++)
			{
				if (Gaia.ReadNext(fileStream2, gaia, i) && (Gaia.gaiaVersionOfStar != 1 || !((Gaia.Dec_deg > num6) | (Gaia.Dec_deg < num7))))
				{
					lstStars.get_Items().Add((object)Gaia.Gaia_ASCII_line);
					Counter++;
					if (Counter % 5 == 0)
					{
						lstStars.get_Items().Add((object)"");
					}
				}
			}
		}

		private void cmdTycho2_Click(object sender, EventArgs e)
		{
			GetTycho_User(useTycho: true, "");
		}

		private void cmdUser_Click(object sender, EventArgs e)
		{
			GetTycho_User(useTycho: false, cmbUserCat.get_Items().get_Item(((ListControl)cmbUserCat).get_SelectedIndex()).ToString());
		}

		private void GetTycho_User(bool useTycho, string BaseFileName)
		{
			lstStars.get_Items().Clear();
			Tycho2.RecordLength = 27;
			FileStream fileStream;
			BinaryReader binaryReader;
			int num;
			FileStream fileStream2;
			if (useTycho)
			{
				fileStream = new FileStream(AppPath + "\\Resource Files\\Tycho2.inx", FileMode.Open, FileAccess.Read);
				binaryReader = new BinaryReader(fileStream);
				num = 361 * (179 - ((ListControl)cmbTycho2_Dec).get_SelectedIndex()) + ((ListControl)cmbTycho2_RA).get_SelectedIndex();
				fileStream2 = new FileStream(AppPath + "\\Resource Files\\Tycho2.bin", FileMode.Open, FileAccess.Read);
				if (fileStream2.Length > 99000000)
				{
					Tycho2.RecordLength = 41;
				}
			}
			else
			{
				fileStream = new FileStream(AppPath + "\\Resource Files\\" + BaseFileName + ".inx", FileMode.Open, FileAccess.Read);
				binaryReader = new BinaryReader(fileStream);
				num = 361 * (179 - ((ListControl)cmbUser_Dec).get_SelectedIndex()) + ((ListControl)cmbUser_RA).get_SelectedIndex();
				fileStream2 = new FileStream(AppPath + "\\Resource Files\\" + BaseFileName + ".bin", FileMode.Open, FileAccess.Read);
			}
			fileStream.Seek(4 * num, SeekOrigin.Begin);
			int num2 = binaryReader.ReadInt32();
			int num3 = binaryReader.ReadInt32();
			binaryReader.Close();
			BinaryReader binaryReader2 = new BinaryReader(fileStream2);
			string text = "Star ID              Mv     Mb    Right Ascension  pm(RA)    e(RA)  e(PM)   epoch      o  '   \"      pm(Dec)  e(Dec)  e(PM)   epoch    RV    Parallax";
			string text2 = "                                     h  m   s       sec       \"        \"               o  '   \"        \"        \"      \"               km/s     \"";
			((Control)lblColumns).set_Text(text);
			lstStars.get_Items().Add((object)text);
			((Control)lblUnits).set_Text(text2);
			lstStars.get_Items().Add((object)text2);
			Counter = 0;
			for (int i = num2; i < num3; i++)
			{
				Tycho2.ReadNext(fileStream2, binaryReader2, i - 1);
				lstStars.get_Items().Add((object)Tycho2.Tycho2_ASCII_line);
				Counter++;
				if (Counter % 5 == 0)
				{
					lstStars.get_Items().Add((object)"");
				}
			}
			binaryReader2.Close();
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
			Settings.Default.Save_Stars = Output.SaveAppendPredictionText(CollectEvents(), "Star catalogue listing", Settings.Default.Save_Stars);
		}

		private string CollectEvents()
		{
			StringBuilder stringBuilder = new StringBuilder();
			int count = lstStars.get_Items().get_Count();
			for (int i = 0; i < count; i++)
			{
				stringBuilder.AppendLine(lstStars.get_Items().get_Item(i).ToString());
			}
			stringBuilder.AppendLine("");
			return stringBuilder.ToString();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Component)this).Dispose();
		}

		private void GoogleSky_XZ80Q_Click(object sender, EventArgs e)
		{
			double num = 0.5 + (double)((ListControl)cmbXZ80_RA).get_SelectedIndex();
			double dec_deg = 23.44 * Math.Sin(num / 57.295779);
			int width_in_arcMins = 120;
			GoogleEarth.CreateGoogleSkyKMLFile(num, dec_deg, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void GoogleSky_TychoGaia_Click(object sender, EventArgs e)
		{
			double rA_deg = 0.5 + (double)((ListControl)cmbGaia_RA).get_SelectedIndex();
			double dec_deg = -90.0 + ((double)((ListControl)cmbGaia_Dec).get_SelectedIndex() + 0.5) / 2.0;
			int width_in_arcMins = 60;
			GoogleEarth.CreateGoogleSkyKMLFile(rA_deg, dec_deg, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void GoogleSky_Tycho2_Click(object sender, EventArgs e)
		{
			double rA_deg = 0.5 + (double)((ListControl)cmbTycho2_RA).get_SelectedIndex();
			double dec_deg = (double)(-90 + ((ListControl)cmbTycho2_Dec).get_SelectedIndex()) + 0.5;
			int width_in_arcMins = 60;
			GoogleEarth.CreateGoogleSkyKMLFile(rA_deg, dec_deg, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void GoogleSky_User_Click(object sender, EventArgs e)
		{
			double rA_deg = 0.5 + (double)((ListControl)cmbUser_RA).get_SelectedIndex();
			double dec_deg = (double)(-90 + ((ListControl)cmbUser_Dec).get_SelectedIndex()) + 0.5;
			int width_in_arcMins = 60;
			GoogleEarth.CreateGoogleSkyKMLFile(rA_deg, dec_deg, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void GoogleSky_NOMAD_Click(object sender, EventArgs e)
		{
			double rA_deg = 0.1 + 0.2 * (double)((ListControl)cmbNomadRA).get_SelectedIndex();
			double dec_deg = -90.0 + ((double)((ListControl)cmbNomadDec).get_SelectedIndex() + 0.5) / 10.0;
			int width_in_arcMins = 12;
			GoogleEarth.CreateGoogleSkyKMLFile(rA_deg, dec_deg, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Stars - display range");
		}

		private void cmdUCAC4_Click(object sender, EventArgs e)
		{
			lstStars.get_Items().Clear();
			string text = " UCAC ID          Mv    Mb    Mr  D T #c  Right Ascen.  pm(RA)     e(RA)  e(PM)   epoch      Declination    pm(Dec)   e(Dec)  e(PM)  epoch  Paralx  Hip #  Tycho-2 #        MPOS #   UCAC2 #";
			string text2 = "                                          h  m   s        sec        \"     \"                  o  '   \"       \"         \"      \"";
			((Control)lblColumns).set_Text(text);
			lstStars.get_Items().Add((object)text);
			((Control)lblUnits).set_Text(text2);
			lstStars.get_Items().Add((object)text2);
			int selectedIndex = ((ListControl)cmbUCAC4_RA).get_SelectedIndex();
			int decZone = ((ListControl)cmbUCAC4_Dec).get_SelectedIndex() + 1;
			UCAC4.InitialiseUCAC4();
			UCAC4.Open_UCAC4_Catalogue(decZone);
			UCAC4.GetUCAC4IndexAndBin(decZone, selectedIndex, out var StartRecordNum, out var NumInBin);
			int num = StartRecordNum;
			int num2 = StartRecordNum + NumInBin;
			if (NumInBin < 40)
			{
				num2 = num + 40;
			}
			UCAC4.GetUCAC4IndexAndBin(decZone, 1439, out StartRecordNum, out NumInBin);
			int num3 = StartRecordNum + NumInBin;
			if (num2 > num3)
			{
				num2 = num3;
			}
			Counter = 0;
			for (int i = num; i < num2; i++)
			{
				UCAC4.Read_UCAC4_entry(i, chkHip.get_Checked());
				lstStars.get_Items().Add((object)UCAC4.UCAC4_ASCII_line);
				Counter++;
				if (Counter % 5 == 0)
				{
					lstStars.get_Items().Add((object)"");
				}
			}
			UCAC4.Close_UCAC4_Catalogue();
		}

		private void copyToolStripMenuItem1_Click(object sender, EventArgs e)
		{
			try
			{
				Clipboard.SetText(CollectLines());
			}
			catch
			{
			}
		}

		private string CollectLines()
		{
			string text = "";
			for (int i = 0; i < lstStars.get_Items().get_Count(); i++)
			{
				text = text + lstStars.get_Items().get_Item(i).ToString() + "\r\n";
			}
			return text;
		}

		private void Star_DisplayRange_Resize(object sender, EventArgs e)
		{
			((Control)panelCats).set_Width(((Control)this).get_Width() - 19);
			((Control)lstStars).set_Width(((Control)this).get_Width() - 19);
			((Control)lstStars).set_Height(((Control)this).get_Height() - 220);
		}

		private void GoogleSky_UCAC4_Click(object sender, EventArgs e)
		{
			double rA_deg = 0.125 + 0.25 * (double)((ListControl)cmbUCAC4_RA).get_SelectedIndex();
			double dec_deg = -90.0 + ((double)((ListControl)cmbUCAC4_Dec).get_SelectedIndex() + 0.5) / 5.0;
			int width_in_arcMins = 90;
			GoogleEarth.CreateGoogleSkyKMLFile(rA_deg, dec_deg, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void GoogleSky_PPMXL_Click(object sender, EventArgs e)
		{
			double rA_deg = 0.1 + 0.2 * (double)((ListControl)cmbPPMXL_RA).get_SelectedIndex();
			double dec_deg = -90.0 + ((double)((ListControl)cmbPPMXL_Dec).get_SelectedIndex() + 0.5) / 4.0;
			int width_in_arcMins = 90;
			GoogleEarth.CreateGoogleSkyKMLFile(rA_deg, dec_deg, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
		}

		private void cmdPPMXL_Click(object sender, EventArgs e)
		{
			lstStars.get_Items().Clear();
			string text = "PPMXL ID        Mv    Mb    Mr       Right Ascen.  pm(RA)    e(RA)  e(PM)   epoch      Declination    pm(Dec)  e(Dec) e(PM)   epoch";
			string text2 = "                                       h  m   s      sec       \"     \"                  o  '   \"       \"        \"      \"";
			((Control)lblColumns).set_Text(text);
			lstStars.get_Items().Add((object)text);
			((Control)lblUnits).set_Text(text2);
			lstStars.get_Items().Add((object)text2);
			int selectedIndex = ((ListControl)cmbPPMXL_RA).get_SelectedIndex();
			int decZone = ((ListControl)cmbPPMXL_Dec).get_SelectedIndex() + 1;
			PPMXL.InitialisePPMXL();
			PPMXL.Open_PPMXL_Catalogue(decZone);
			int pPMXLIndexValue = PPMXL.GetPPMXLIndexValue(decZone, selectedIndex);
			int num = PPMXL.GetPPMXLIndexValue(decZone, selectedIndex + 1) - 1;
			Counter = 0;
			for (int i = pPMXLIndexValue; i <= num; i++)
			{
				PPMXL.Read_PPMXL_entry(i);
				lstStars.get_Items().Add((object)PPMXL.PPMXL_ASCII_line);
				Counter++;
				if (Counter % 5 == 0)
				{
					lstStars.get_Items().Add((object)"");
				}
			}
			PPMXL.Close_PPMXL_Catalogue();
		}

		private void chkHip_CheckedChanged(object sender, EventArgs e)
		{
			cmdUCAC4_Click(sender, e);
		}

		private void copyStarEntryToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstStars.get_Items().get_Count() >= 2 && ((ListControl)lstStars).get_SelectedIndex() >= 2)
			{
				Clipboard.SetText(lstStars.get_Items().get_Item(0).ToString() + "\r\n" + lstStars.get_Items().get_Item(((ListControl)lstStars).get_SelectedIndex()).ToString());
			}
		}

		private void catalogueComparisonVizieRToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstStars.get_Items().get_Count() >= 2 && ((ListControl)lstStars).get_SelectedIndex() >= 2)
			{
				GetStarCoords(((ListControl)lstStars).get_SelectedIndex(), out var StarID, out var RAh, out var RAm, out var RAs, out var DecD, out var DecM, out var DecS, out var _, out var _);
				GetStarPosition.ShowCompareCatalogues(fromAsteroidPrediction: true);
				Application.DoEvents();
				((Control)GetStarPosition.CompareCatalogues.lblEventDetails).set_Text("Star : " + StarID);
				((Control)GetStarPosition.CompareCatalogues.txtRAh).set_Text(RAh);
				((Control)GetStarPosition.CompareCatalogues.txtRAm).set_Text(RAm);
				((Control)GetStarPosition.CompareCatalogues.txtRAs).set_Text(RAs);
				((Control)GetStarPosition.CompareCatalogues.txtD).set_Text(DecD);
				((Control)GetStarPosition.CompareCatalogues.txtm).set_Text(DecM);
				((Control)GetStarPosition.CompareCatalogues.txts).set_Text(DecS);
				((Control)GetStarPosition.CompareCatalogues.txtMv).set_Text("...");
				((Control)GetStarPosition.CompareCatalogues.txtMp).set_Text("...");
				GetStarPosition.CompareCatalogues.updnDiameter.set_Value(20m);
				((Control)GetStarPosition.CompareCatalogues.txtDuration).set_Text("");
				GetStarPosition.CompareCatalogues.updnMotionPA.set_Value(90m);
				GetStarPosition.CompareCatalogues.chkShowMotion.set_Checked(false);
				GetStarPosition.CompareCatalogues.updnEpoch.set_Value(2000m);
				decimal value = (decimal)Utilities.BesselianYear(Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day));
				GetStarPosition.CompareCatalogues.updnEpoch.set_Value(value);
				Application.DoEvents();
				GetStarPosition.CompareCatalogues.FindInSpecificCatalogues(DownLoadVizier: true);
			}
		}

		private void displayInGoogleSkyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (lstStars.get_Items().get_Count() >= 2 && ((ListControl)lstStars).get_SelectedIndex() >= 2)
			{
				GetStarCoords(((ListControl)lstStars).get_SelectedIndex(), out var _, out var _, out var _, out var _, out var _, out var _, out var _, out var RA, out var Dec);
				int width_in_arcMins = 10;
				GoogleEarth.CreateGoogleSkyKMLFile(15.0 * RA, Dec, width_in_arcMins, AppPath + "\\AutoGenerated Asteroids\\ViewGoogleSky.KML");
			}
		}

		private void GetStarCoords(int Line, out string StarID, out string RAh, out string RAm, out string RAs, out string DecD, out string DecM, out string DecS, out double RA, out double Dec)
		{
			string text = lstStars.get_Items().get_Item(0).ToString();
			string? text2 = lstStars.get_Items().get_Item(1).ToString();
			double num = 0.0;
			double result = 2000.0;
			string text3 = lstStars.get_Items().get_Item(Line).ToString();
			int num2 = text3.IndexOf(" ", 5);
			if (num2 < 6)
			{
				num2 = text3.IndexOf(" ", num2 + 1);
			}
			StarID = text3.Substring(0, num2);
			int startIndex = text2!.IndexOf("h  m") - 1;
			int startIndex2 = text.IndexOf("pm RA") - 2;
			int startIndex3 = text2!.IndexOf("o  '") - 2;
			int startIndex4 = text.IndexOf("pm Dec") - 2;
			int num3 = text.IndexOf("epoch") - 1;
			RA = Utilities.DMStoDeg("  " + text3.Substring(startIndex, 13));
			Dec = Utilities.DMStoDeg(" " + text3.Substring(startIndex3, 13));
			if (text.ToUpper().Contains("URAT") && num3 > 0 && !double.TryParse(text3.Substring(num3, 7), out result))
			{
				result = 2000.0;
			}
			if (!double.TryParse(text3.Substring(startIndex2, 8), out var result2))
			{
				result2 = 0.0;
			}
			if (!double.TryParse(text3.Substring(startIndex4, 8), out var result3))
			{
				result3 = 0.0;
			}
			double num4 = Utilities.BesselianYear(Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day));
			num = RA + result2 * (num4 - result) / 3600.0;
			double degree = Dec + result3 * (num4 - result) / 3600.0;
			string text4 = Utilities.DEGtoDMS(num, 2, 4, MinutesOnly: false);
			string text5 = Utilities.DEGtoDMS(degree, 3, 3, MinutesOnly: false);
			RAh = text4.Substring(0, 2);
			RAm = text4.Substring(3, 2);
			RAs = text4.Substring(6, 7);
			DecD = text5.Substring(0, 3);
			DecM = text5.Substring(4, 2);
			DecS = text5.Substring(7, 6);
		}

		private void lstStars_MouseDown(object sender, MouseEventArgs e)
		{
			//IL_0001: Unknown result type (might be due to invalid IL or missing references)
			//IL_000b: Invalid comparison between Unknown and I4
			if ((int)e.get_Button() == 2097152)
			{
				int num = lstStars.IndexFromPoint(e.get_X(), e.get_Y());
				if (num >= 0 && num < lstStars.get_Items().get_Count())
				{
					((ListControl)lstStars).set_SelectedIndex(num);
				}
				((Control)lstStars).Refresh();
			}
		}

		private void lblGaiaCat_Click(object sender, EventArgs e)
		{
			((Control)lstGaiaFiles).set_Visible(true);
		}

		private void lstGaiaFiles_DoubleClick(object sender, EventArgs e)
		{
			if ((((ListControl)lstGaiaFiles).get_SelectedIndex() > 0) & !lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString()!.Contains("____"))
			{
				((Control)lblGaiaCat).set_Text(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString());
				((Control)lblGaiaCat).set_BackColor(Color.DarkGreen);
			}
			else
			{
				((Control)lblGaiaCat).set_Text("Click to select Gaia Cat");
			}
			((Control)lstGaiaFiles).set_Visible(false);
		}

		private void lstGaiaFiles_Click(object sender, EventArgs e)
		{
			if ((((ListControl)lstGaiaFiles).get_SelectedIndex() == 1) & (lstGaiaFiles.get_Items().get_Count() > 2))
			{
				((ListControl)lstGaiaFiles).set_SelectedIndex(2);
			}
			if (((ListControl)lstGaiaFiles).get_SelectedIndex() == 2 + Gaia.GaiaPrimaryFiles.Count)
			{
				if (Gaia.GaiaPrimaryFiles.Count > 0)
				{
					ListBox obj = lstGaiaFiles;
					int selectedIndex = ((ListControl)obj).get_SelectedIndex();
					((ListControl)obj).set_SelectedIndex(selectedIndex + 1);
				}
				else if (((ListControl)lstGaiaFiles).get_SelectedIndex() > 2)
				{
					ListBox obj2 = lstGaiaFiles;
					int selectedIndex = ((ListControl)obj2).get_SelectedIndex();
					((ListControl)obj2).set_SelectedIndex(selectedIndex - 1);
				}
			}
			if ((((ListControl)lstGaiaFiles).get_SelectedIndex() > 0) & !lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString()!.Contains("____"))
			{
				Gaia.ShowGaiaMap();
				((Control)Gaia.GaiaMap).set_Left(((Control)this).get_Left() + 300);
				((Control)Gaia.GaiaMap).set_Top(((Control)this).get_Top() + 100);
				Gaia.GaiaMap.MapCatalogueCoverage(lstGaiaFiles.get_Items().get_Item(((ListControl)lstGaiaFiles).get_SelectedIndex()).ToString());
			}
		}

		private void lstGaiaFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstGaiaFiles).get_SelectedIndex() == 1)
			{
				((ListControl)lstGaiaFiles).set_SelectedIndex(2);
			}
		}

		private void lstGaiaFiles_Leave(object sender, EventArgs e)
		{
			try
			{
				((Form)Gaia.GaiaMap).Close();
				((Component)(object)Gaia.GaiaMap).Dispose();
			}
			catch
			{
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
			//IL_01c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_01cf: Expected O, but got Unknown
			//IL_01d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_01da: Expected O, but got Unknown
			//IL_01db: Unknown result type (might be due to invalid IL or missing references)
			//IL_01e5: Expected O, but got Unknown
			//IL_01e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_01f0: Expected O, but got Unknown
			//IL_01f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_01fb: Expected O, but got Unknown
			//IL_01fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0206: Expected O, but got Unknown
			//IL_0207: Unknown result type (might be due to invalid IL or missing references)
			//IL_0211: Expected O, but got Unknown
			//IL_0212: Unknown result type (might be due to invalid IL or missing references)
			//IL_021c: Expected O, but got Unknown
			//IL_021d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0227: Expected O, but got Unknown
			//IL_0228: Unknown result type (might be due to invalid IL or missing references)
			//IL_0232: Expected O, but got Unknown
			//IL_0233: Unknown result type (might be due to invalid IL or missing references)
			//IL_023d: Expected O, but got Unknown
			//IL_023e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0248: Expected O, but got Unknown
			//IL_0249: Unknown result type (might be due to invalid IL or missing references)
			//IL_0253: Expected O, but got Unknown
			//IL_0254: Unknown result type (might be due to invalid IL or missing references)
			//IL_025e: Expected O, but got Unknown
			//IL_025f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0269: Expected O, but got Unknown
			//IL_026a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0274: Expected O, but got Unknown
			//IL_0275: Unknown result type (might be due to invalid IL or missing references)
			//IL_027f: Expected O, but got Unknown
			//IL_0280: Unknown result type (might be due to invalid IL or missing references)
			//IL_028a: Expected O, but got Unknown
			//IL_028b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0295: Expected O, but got Unknown
			//IL_0296: Unknown result type (might be due to invalid IL or missing references)
			//IL_02a0: Expected O, but got Unknown
			//IL_02a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_02ab: Expected O, but got Unknown
			//IL_02ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_02b6: Expected O, but got Unknown
			//IL_02b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_02c1: Expected O, but got Unknown
			//IL_02c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_02cc: Expected O, but got Unknown
			//IL_02cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_02d7: Expected O, but got Unknown
			//IL_02d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_02e2: Expected O, but got Unknown
			//IL_0412: Unknown result type (might be due to invalid IL or missing references)
			//IL_041c: Expected O, but got Unknown
			//IL_1c0c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1c16: Expected O, but got Unknown
			//IL_2997: Unknown result type (might be due to invalid IL or missing references)
			//IL_29a1: Expected O, but got Unknown
			components = new Container();
			lstStars = new ListBox();
			contextMenuStrip1 = new ContextMenuStrip(components);
			copyStarEntryToolStripMenuItem = new ToolStripMenuItem();
			catalogueComparisonVizieRToolStripMenuItem = new ToolStripMenuItem();
			displayInGoogleSkyToolStripMenuItem = new ToolStripMenuItem();
			grpNomad = new GroupBox();
			GoogleSky_NOMAD = new Button();
			label4 = new Label();
			cmdNomad = new Button();
			cmbNomadDec = new ComboBox();
			cmbNomadRA = new ComboBox();
			grpTycho2 = new GroupBox();
			GoogleSky_Tycho2 = new Button();
			label1 = new Label();
			cmdTycho2 = new Button();
			cmbTycho2_Dec = new ComboBox();
			cmbTycho2_RA = new ComboBox();
			grpUser = new GroupBox();
			GoogleSky_User = new Button();
			cmbUserCat = new ComboBox();
			label2 = new Label();
			cmdUser = new Button();
			cmbUser_Dec = new ComboBox();
			cmbUser_RA = new ComboBox();
			grpXZ80Q = new GroupBox();
			GoogleSky_XZ80Q = new Button();
			label5 = new Label();
			cmdXZ80Q = new Button();
			cmbXZ80_RA = new ComboBox();
			menuStrip1 = new MenuStrip();
			withListToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem1 = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			withListOfStarsToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printPreviewToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			toolTip1 = new ToolTip(components);
			GoogleSky_UCAC4 = new Button();
			GoogleSky_PPMXL = new Button();
			GoogleSky_TychoGaia = new Button();
			grpUCAC4 = new GroupBox();
			chkHip = new CheckBox();
			label7 = new Label();
			cmdUCAC4 = new Button();
			cmbUCAC4_Dec = new ComboBox();
			cmbUCAC4_RA = new ComboBox();
			grpPPMXL = new GroupBox();
			label8 = new Label();
			cmdPPMXL = new Button();
			cmbPPMXL_Dec = new ComboBox();
			cmbPPMXL_RA = new ComboBox();
			panelCats = new Panel();
			grpGaia = new GroupBox();
			lblGaiaCat = new Label();
			label11 = new Label();
			cmdGaia = new Button();
			cmbGaia_Dec = new ComboBox();
			cmbGaia_RA = new ComboBox();
			label10 = new Label();
			lstGaiaFiles = new ListBox();
			lblColumns = new Label();
			lblUnits = new Label();
			((Control)contextMenuStrip1).SuspendLayout();
			((Control)grpNomad).SuspendLayout();
			((Control)grpTycho2).SuspendLayout();
			((Control)grpUser).SuspendLayout();
			((Control)grpXZ80Q).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)grpUCAC4).SuspendLayout();
			((Control)grpPPMXL).SuspendLayout();
			((Control)panelCats).SuspendLayout();
			((Control)grpGaia).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstStars).set_ContextMenuStrip(contextMenuStrip1);
			((Control)lstStars).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstStars).set_FormattingEnabled(true);
			lstStars.set_HorizontalExtent(1400);
			lstStars.set_HorizontalScrollbar(true);
			lstStars.set_ItemHeight(14);
			((Control)lstStars).set_Location(new Point(1, 180));
			((Control)lstStars).set_Name("lstStars");
			((Control)lstStars).set_Size(new Size(1128, 438));
			((Control)lstStars).set_TabIndex(2);
			((Control)lstStars).add_MouseDown(new MouseEventHandler(lstStars_MouseDown));
			((ToolStrip)contextMenuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyStarEntryToolStripMenuItem,
				(ToolStripItem)catalogueComparisonVizieRToolStripMenuItem,
				(ToolStripItem)displayInGoogleSkyToolStripMenuItem
			});
			((Control)contextMenuStrip1).set_Name("contextMenuStrip1");
			((Control)contextMenuStrip1).set_Size(new Size(229, 70));
			((ToolStripItem)copyStarEntryToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyStarEntryToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyStarEntryToolStripMenuItem).set_Name("copyStarEntryToolStripMenuItem");
			((ToolStripItem)copyStarEntryToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)copyStarEntryToolStripMenuItem).set_Text("Copy star entry");
			((ToolStripItem)copyStarEntryToolStripMenuItem).add_Click((EventHandler)copyStarEntryToolStripMenuItem_Click);
			((ToolStripItem)catalogueComparisonVizieRToolStripMenuItem).set_Image((Image)Resources.vizier_40x35);
			((ToolStripItem)catalogueComparisonVizieRToolStripMenuItem).set_Name("catalogueComparisonVizieRToolStripMenuItem");
			((ToolStripItem)catalogueComparisonVizieRToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)catalogueComparisonVizieRToolStripMenuItem).set_Text("Catalogue comparison VizieR");
			((ToolStripItem)catalogueComparisonVizieRToolStripMenuItem).add_Click((EventHandler)catalogueComparisonVizieRToolStripMenuItem_Click);
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Image((Image)Resources.google_earth);
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Name("displayInGoogleSkyToolStripMenuItem");
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Size(new Size(228, 22));
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).set_Text("Display in GoogleSky");
			((ToolStripItem)displayInGoogleSkyToolStripMenuItem).add_Click((EventHandler)displayInGoogleSkyToolStripMenuItem_Click);
			((Control)grpNomad).get_Controls().Add((Control)(object)GoogleSky_NOMAD);
			((Control)grpNomad).get_Controls().Add((Control)(object)label4);
			((Control)grpNomad).get_Controls().Add((Control)(object)cmdNomad);
			((Control)grpNomad).get_Controls().Add((Control)(object)cmbNomadDec);
			((Control)grpNomad).get_Controls().Add((Control)(object)cmbNomadRA);
			((Control)grpNomad).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpNomad).set_Location(new Point(730, 2));
			((Control)grpNomad).set_Name("grpNomad");
			((Control)grpNomad).set_Size(new Size(143, 90));
			((Control)grpNomad).set_TabIndex(7);
			grpNomad.set_TabStop(false);
			((Control)grpNomad).set_Text("NOMAD");
			((Control)GoogleSky_NOMAD).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky_NOMAD).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky_NOMAD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)GoogleSky_NOMAD).set_Location(new Point(98, 59));
			((Control)GoogleSky_NOMAD).set_Name("GoogleSky_NOMAD");
			((Control)GoogleSky_NOMAD).set_Size(new Size(26, 25));
			((Control)GoogleSky_NOMAD).set_TabIndex(4);
			toolTip1.SetToolTip((Control)(object)GoogleSky_NOMAD, "Plot on GoogleSky");
			((ButtonBase)GoogleSky_NOMAD).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky_NOMAD).add_Click((EventHandler)GoogleSky_NOMAD_Click);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(9, 18));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(124, 13));
			((Control)label4).set_TabIndex(0);
			((Control)label4).set_Text("R.A. step         Dec zone");
			((Control)cmdNomad).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdNomad).set_Location(new Point(19, 59));
			((Control)cmdNomad).set_Name("cmdNomad");
			((Control)cmdNomad).set_Size(new Size(53, 25));
			((Control)cmdNomad).set_TabIndex(3);
			((Control)cmdNomad).set_Text("Display");
			((ButtonBase)cmdNomad).set_UseVisualStyleBackColor(true);
			((Control)cmdNomad).add_Click((EventHandler)cmdNomad_Click);
			cmbNomadDec.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbNomadDec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbNomadDec).set_FormattingEnabled(true);
			((Control)cmbNomadDec).set_Location(new Point(79, 33));
			((Control)cmbNomadDec).set_Name("cmbNomadDec");
			((Control)cmbNomadDec).set_Size(new Size(56, 21));
			((Control)cmbNomadDec).set_TabIndex(2);
			cmbNomadRA.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbNomadRA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbNomadRA).set_FormattingEnabled(true);
			((Control)cmbNomadRA).set_Location(new Point(7, 33));
			((Control)cmbNomadRA).set_Name("cmbNomadRA");
			((Control)cmbNomadRA).set_Size(new Size(68, 21));
			((Control)cmbNomadRA).set_TabIndex(1);
			((Control)grpTycho2).get_Controls().Add((Control)(object)GoogleSky_Tycho2);
			((Control)grpTycho2).get_Controls().Add((Control)(object)label1);
			((Control)grpTycho2).get_Controls().Add((Control)(object)cmdTycho2);
			((Control)grpTycho2).get_Controls().Add((Control)(object)cmbTycho2_Dec);
			((Control)grpTycho2).get_Controls().Add((Control)(object)cmbTycho2_RA);
			((Control)grpTycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpTycho2).set_Location(new Point(577, 2));
			((Control)grpTycho2).set_Name("grpTycho2");
			((Control)grpTycho2).set_Size(new Size(143, 90));
			((Control)grpTycho2).set_TabIndex(1);
			grpTycho2.set_TabStop(false);
			((Control)grpTycho2).set_Text("Tycho-2");
			((Control)GoogleSky_Tycho2).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky_Tycho2).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky_Tycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)GoogleSky_Tycho2).set_Location(new Point(95, 59));
			((Control)GoogleSky_Tycho2).set_Name("GoogleSky_Tycho2");
			((Control)GoogleSky_Tycho2).set_Size(new Size(26, 25));
			((Control)GoogleSky_Tycho2).set_TabIndex(4);
			toolTip1.SetToolTip((Control)(object)GoogleSky_Tycho2, "Plot on GoogleSky");
			((ButtonBase)GoogleSky_Tycho2).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky_Tycho2).add_Click((EventHandler)GoogleSky_Tycho2_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(8, 18));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(124, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("R.A. step         Dec zone");
			((Control)cmdTycho2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdTycho2).set_Location(new Point(16, 59));
			((Control)cmdTycho2).set_Name("cmdTycho2");
			((Control)cmdTycho2).set_Size(new Size(53, 25));
			((Control)cmdTycho2).set_TabIndex(3);
			((Control)cmdTycho2).set_Text("Display");
			((ButtonBase)cmdTycho2).set_UseVisualStyleBackColor(true);
			((Control)cmdTycho2).add_Click((EventHandler)cmdTycho2_Click);
			cmbTycho2_Dec.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTycho2_Dec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTycho2_Dec).set_FormattingEnabled(true);
			((Control)cmbTycho2_Dec).set_Location(new Point(80, 33));
			cmbTycho2_Dec.set_MaxDropDownItems(30);
			((Control)cmbTycho2_Dec).set_Name("cmbTycho2_Dec");
			((Control)cmbTycho2_Dec).set_Size(new Size(56, 21));
			((Control)cmbTycho2_Dec).set_TabIndex(2);
			cmbTycho2_RA.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTycho2_RA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTycho2_RA).set_FormattingEnabled(true);
			((Control)cmbTycho2_RA).set_Location(new Point(8, 33));
			cmbTycho2_RA.set_MaxDropDownItems(30);
			((Control)cmbTycho2_RA).set_Name("cmbTycho2_RA");
			((Control)cmbTycho2_RA).set_Size(new Size(68, 21));
			((Control)cmbTycho2_RA).set_TabIndex(1);
			((Control)grpUser).get_Controls().Add((Control)(object)GoogleSky_User);
			((Control)grpUser).get_Controls().Add((Control)(object)cmbUserCat);
			((Control)grpUser).get_Controls().Add((Control)(object)label2);
			((Control)grpUser).get_Controls().Add((Control)(object)cmdUser);
			((Control)grpUser).get_Controls().Add((Control)(object)cmbUser_Dec);
			((Control)grpUser).get_Controls().Add((Control)(object)cmbUser_RA);
			((Control)grpUser).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpUser).set_Location(new Point(884, 2));
			((Control)grpUser).set_Name("grpUser");
			((Control)grpUser).set_Size(new Size(143, 90));
			((Control)grpUser).set_TabIndex(8);
			grpUser.set_TabStop(false);
			((Control)grpUser).set_Text("User  catalogues");
			((Control)GoogleSky_User).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky_User).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky_User).set_Location(new Point(147, 59));
			((Control)GoogleSky_User).set_Name("GoogleSky_User");
			((Control)GoogleSky_User).set_Size(new Size(26, 25));
			((Control)GoogleSky_User).set_TabIndex(5);
			toolTip1.SetToolTip((Control)(object)GoogleSky_User, "Plot on GoogleSky");
			((ButtonBase)GoogleSky_User).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky_User).add_Click((EventHandler)GoogleSky_User_Click);
			cmbUserCat.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbUserCat).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbUserCat).set_FormattingEnabled(true);
			((Control)cmbUserCat).set_Location(new Point(9, 60));
			((Control)cmbUserCat).set_Name("cmbUserCat");
			((Control)cmbUserCat).set_Size(new Size(58, 21));
			((Control)cmbUserCat).set_TabIndex(3);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(9, 18));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(124, 13));
			((Control)label2).set_TabIndex(0);
			((Control)label2).set_Text("R.A. step         Dec zone");
			((Control)cmdUser).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUser).set_Location(new Point(80, 59));
			((Control)cmdUser).set_Name("cmdUser");
			((Control)cmdUser).set_Size(new Size(53, 25));
			((Control)cmdUser).set_TabIndex(4);
			((Control)cmdUser).set_Text("Display");
			((ButtonBase)cmdUser).set_UseVisualStyleBackColor(true);
			((Control)cmdUser).add_Click((EventHandler)cmdUser_Click);
			cmbUser_Dec.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbUser_Dec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbUser_Dec).set_FormattingEnabled(true);
			((Control)cmbUser_Dec).set_Location(new Point(79, 33));
			cmbUser_Dec.set_MaxDropDownItems(30);
			((Control)cmbUser_Dec).set_Name("cmbUser_Dec");
			((Control)cmbUser_Dec).set_Size(new Size(56, 21));
			((Control)cmbUser_Dec).set_TabIndex(2);
			cmbUser_RA.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbUser_RA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbUser_RA).set_FormattingEnabled(true);
			((Control)cmbUser_RA).set_Location(new Point(7, 33));
			cmbUser_RA.set_MaxDropDownItems(30);
			((Control)cmbUser_RA).set_Name("cmbUser_RA");
			((Control)cmbUser_RA).set_Size(new Size(68, 21));
			((Control)cmbUser_RA).set_TabIndex(1);
			((Control)grpXZ80Q).get_Controls().Add((Control)(object)GoogleSky_XZ80Q);
			((Control)grpXZ80Q).get_Controls().Add((Control)(object)label5);
			((Control)grpXZ80Q).get_Controls().Add((Control)(object)cmdXZ80Q);
			((Control)grpXZ80Q).get_Controls().Add((Control)(object)cmbXZ80_RA);
			((Control)grpXZ80Q).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpXZ80Q).set_Location(new Point(3, 2));
			((Control)grpXZ80Q).set_Name("grpXZ80Q");
			((Control)grpXZ80Q).set_Size(new Size(114, 90));
			((Control)grpXZ80Q).set_TabIndex(0);
			grpXZ80Q.set_TabStop(false);
			((Control)grpXZ80Q).set_Text("XZ80Q");
			((Control)GoogleSky_XZ80Q).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky_XZ80Q).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky_XZ80Q).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)GoogleSky_XZ80Q).set_Location(new Point(80, 59));
			((Control)GoogleSky_XZ80Q).set_Name("GoogleSky_XZ80Q");
			((Control)GoogleSky_XZ80Q).set_Size(new Size(26, 25));
			((Control)GoogleSky_XZ80Q).set_TabIndex(3);
			toolTip1.SetToolTip((Control)(object)GoogleSky_XZ80Q, "Plot on GoogleSky");
			((ButtonBase)GoogleSky_XZ80Q).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky_XZ80Q).add_Click((EventHandler)GoogleSky_XZ80Q_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(26, 18));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(51, 13));
			((Control)label5).set_TabIndex(0);
			((Control)label5).set_Text("R.A. step");
			((Control)cmdXZ80Q).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdXZ80Q).set_Location(new Point(8, 59));
			((Control)cmdXZ80Q).set_Name("cmdXZ80Q");
			((Control)cmdXZ80Q).set_Size(new Size(53, 25));
			((Control)cmdXZ80Q).set_TabIndex(2);
			((Control)cmdXZ80Q).set_Text("Display");
			((ButtonBase)cmdXZ80Q).set_UseVisualStyleBackColor(true);
			((Control)cmdXZ80Q).add_Click((EventHandler)cmdXZ80Q_Click);
			cmbXZ80_RA.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbXZ80_RA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbXZ80_RA).set_FormattingEnabled(true);
			((Control)cmbXZ80_RA).set_Location(new Point(23, 33));
			cmbXZ80_RA.set_MaxDropDownItems(30);
			((Control)cmbXZ80_RA).set_Name("cmbXZ80_RA");
			((Control)cmbXZ80_RA).set_Size(new Size(68, 21));
			((Control)cmbXZ80_RA).set_TabIndex(1);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)withListToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(1135, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withListToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[1] { (ToolStripItem)copyToolStripMenuItem1 });
			((ToolStripItem)withListToolStripMenuItem).set_Name("withListToolStripMenuItem");
			((ToolStripItem)withListToolStripMenuItem).set_Size(new Size(84, 20));
			((ToolStripItem)withListToolStripMenuItem).set_Text("with List...    ");
			((ToolStripItem)copyToolStripMenuItem1).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem1).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem1).set_Name("copyToolStripMenuItem1");
			copyToolStripMenuItem1.set_ShortcutKeys((Keys)131139);
			((ToolStripItem)copyToolStripMenuItem1).set_Size(new Size(144, 22));
			((ToolStripItem)copyToolStripMenuItem1).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem1).add_Click((EventHandler)copyToolStripMenuItem1_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(60, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripDropDownItem)withListOfStarsToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printPreviewToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withListOfStarsToolStripMenuItem).set_Name("withListOfStarsToolStripMenuItem");
			((ToolStripItem)withListOfStarsToolStripMenuItem).set_Size(new Size(117, 20));
			((ToolStripItem)withListOfStarsToolStripMenuItem).set_Text("with List of Stars....");
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
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(37, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)GoogleSky_UCAC4).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky_UCAC4).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky_UCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)GoogleSky_UCAC4).set_Location(new Point(109, 59));
			((Control)GoogleSky_UCAC4).set_Name("GoogleSky_UCAC4");
			((Control)GoogleSky_UCAC4).set_Size(new Size(26, 25));
			((Control)GoogleSky_UCAC4).set_TabIndex(4);
			toolTip1.SetToolTip((Control)(object)GoogleSky_UCAC4, "Plot on GoogleSky");
			((ButtonBase)GoogleSky_UCAC4).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky_UCAC4).add_Click((EventHandler)GoogleSky_UCAC4_Click);
			((Control)GoogleSky_PPMXL).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky_PPMXL).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky_PPMXL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)GoogleSky_PPMXL).set_Location(new Point(98, 59));
			((Control)GoogleSky_PPMXL).set_Name("GoogleSky_PPMXL");
			((Control)GoogleSky_PPMXL).set_Size(new Size(26, 25));
			((Control)GoogleSky_PPMXL).set_TabIndex(4);
			toolTip1.SetToolTip((Control)(object)GoogleSky_PPMXL, "Plot on GoogleSky");
			((ButtonBase)GoogleSky_PPMXL).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky_PPMXL).add_Click((EventHandler)GoogleSky_PPMXL_Click);
			((Control)GoogleSky_TychoGaia).set_BackgroundImage((Image)Resources.sky_mo);
			((Control)GoogleSky_TychoGaia).set_BackgroundImageLayout((ImageLayout)2);
			((Control)GoogleSky_TychoGaia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)GoogleSky_TychoGaia).set_Location(new Point(95, 62));
			((Control)GoogleSky_TychoGaia).set_Name("GoogleSky_TychoGaia");
			((Control)GoogleSky_TychoGaia).set_Size(new Size(26, 25));
			((Control)GoogleSky_TychoGaia).set_TabIndex(4);
			toolTip1.SetToolTip((Control)(object)GoogleSky_TychoGaia, "Plot on GoogleSky");
			((ButtonBase)GoogleSky_TychoGaia).set_UseVisualStyleBackColor(true);
			((Control)GoogleSky_TychoGaia).add_Click((EventHandler)GoogleSky_TychoGaia_Click);
			((Control)grpUCAC4).get_Controls().Add((Control)(object)chkHip);
			((Control)grpUCAC4).get_Controls().Add((Control)(object)GoogleSky_UCAC4);
			((Control)grpUCAC4).get_Controls().Add((Control)(object)label7);
			((Control)grpUCAC4).get_Controls().Add((Control)(object)cmdUCAC4);
			((Control)grpUCAC4).get_Controls().Add((Control)(object)cmbUCAC4_Dec);
			((Control)grpUCAC4).get_Controls().Add((Control)(object)cmbUCAC4_RA);
			((Control)grpUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpUCAC4).set_Location(new Point(276, 2));
			((Control)grpUCAC4).set_Name("grpUCAC4");
			((Control)grpUCAC4).set_Size(new Size(143, 90));
			((Control)grpUCAC4).set_TabIndex(2);
			grpUCAC4.set_TabStop(false);
			((Control)grpUCAC4).set_Text("UCAC4");
			((Control)chkHip).set_AutoSize(true);
			chkHip.set_Checked(Settings.Default.UCAC4UseHip);
			((Control)chkHip).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "UCAC4UseHip", true, (DataSourceUpdateMode)1));
			((Control)chkHip).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkHip).set_Location(new Point(64, 56));
			((Control)chkHip).set_Name("chkHip");
			((Control)chkHip).set_Size(new Size(43, 30));
			((Control)chkHip).set_TabIndex(5);
			((Control)chkHip).set_Text("use\r\nHip");
			((ButtonBase)chkHip).set_UseVisualStyleBackColor(true);
			chkHip.add_CheckedChanged((EventHandler)chkHip_CheckedChanged);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(9, 18));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(124, 13));
			((Control)label7).set_TabIndex(0);
			((Control)label7).set_Text("R.A. step         Dec zone");
			((Control)cmdUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUCAC4).set_Location(new Point(7, 59));
			((Control)cmdUCAC4).set_Name("cmdUCAC4");
			((Control)cmdUCAC4).set_Size(new Size(53, 25));
			((Control)cmdUCAC4).set_TabIndex(3);
			((Control)cmdUCAC4).set_Text("Display");
			((ButtonBase)cmdUCAC4).set_UseVisualStyleBackColor(true);
			((Control)cmdUCAC4).add_Click((EventHandler)cmdUCAC4_Click);
			cmbUCAC4_Dec.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbUCAC4_Dec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbUCAC4_Dec).set_FormattingEnabled(true);
			((Control)cmbUCAC4_Dec).set_Location(new Point(79, 33));
			cmbUCAC4_Dec.set_MaxDropDownItems(30);
			((Control)cmbUCAC4_Dec).set_Name("cmbUCAC4_Dec");
			((Control)cmbUCAC4_Dec).set_Size(new Size(56, 21));
			((Control)cmbUCAC4_Dec).set_TabIndex(2);
			cmbUCAC4_RA.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbUCAC4_RA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbUCAC4_RA).set_FormattingEnabled(true);
			((Control)cmbUCAC4_RA).set_Location(new Point(7, 33));
			cmbUCAC4_RA.set_MaxDropDownItems(30);
			((Control)cmbUCAC4_RA).set_Name("cmbUCAC4_RA");
			((Control)cmbUCAC4_RA).set_Size(new Size(68, 21));
			((Control)cmbUCAC4_RA).set_TabIndex(1);
			((Control)grpPPMXL).get_Controls().Add((Control)(object)GoogleSky_PPMXL);
			((Control)grpPPMXL).get_Controls().Add((Control)(object)label8);
			((Control)grpPPMXL).get_Controls().Add((Control)(object)cmdPPMXL);
			((Control)grpPPMXL).get_Controls().Add((Control)(object)cmbPPMXL_Dec);
			((Control)grpPPMXL).get_Controls().Add((Control)(object)cmbPPMXL_RA);
			((Control)grpPPMXL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpPPMXL).set_Location(new Point(426, 2));
			((Control)grpPPMXL).set_Name("grpPPMXL");
			((Control)grpPPMXL).set_Size(new Size(143, 90));
			((Control)grpPPMXL).set_TabIndex(4);
			grpPPMXL.set_TabStop(false);
			((Control)grpPPMXL).set_Text("PPMXL");
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(9, 18));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(124, 13));
			((Control)label8).set_TabIndex(0);
			((Control)label8).set_Text("R.A. step         Dec zone");
			((Control)cmdPPMXL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPPMXL).set_Location(new Point(19, 59));
			((Control)cmdPPMXL).set_Name("cmdPPMXL");
			((Control)cmdPPMXL).set_Size(new Size(53, 25));
			((Control)cmdPPMXL).set_TabIndex(3);
			((Control)cmdPPMXL).set_Text("Display");
			((ButtonBase)cmdPPMXL).set_UseVisualStyleBackColor(true);
			((Control)cmdPPMXL).add_Click((EventHandler)cmdPPMXL_Click);
			cmbPPMXL_Dec.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbPPMXL_Dec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPPMXL_Dec).set_FormattingEnabled(true);
			((Control)cmbPPMXL_Dec).set_Location(new Point(79, 33));
			((Control)cmbPPMXL_Dec).set_Name("cmbPPMXL_Dec");
			((Control)cmbPPMXL_Dec).set_Size(new Size(56, 21));
			((Control)cmbPPMXL_Dec).set_TabIndex(2);
			cmbPPMXL_RA.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbPPMXL_RA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbPPMXL_RA).set_FormattingEnabled(true);
			((Control)cmbPPMXL_RA).set_Location(new Point(7, 33));
			((Control)cmbPPMXL_RA).set_Name("cmbPPMXL_RA");
			((Control)cmbPPMXL_RA).set_Size(new Size(68, 21));
			((Control)cmbPPMXL_RA).set_TabIndex(1);
			((ScrollableControl)panelCats).set_AutoScroll(true);
			((ScrollableControl)panelCats).set_AutoScrollMargin(new Size(10, 0));
			panelCats.set_AutoSizeMode((AutoSizeMode)0);
			panelCats.set_BorderStyle((BorderStyle)2);
			((Control)panelCats).get_Controls().Add((Control)(object)grpGaia);
			((Control)panelCats).get_Controls().Add((Control)(object)grpPPMXL);
			((Control)panelCats).get_Controls().Add((Control)(object)grpUCAC4);
			((Control)panelCats).get_Controls().Add((Control)(object)grpUser);
			((Control)panelCats).get_Controls().Add((Control)(object)grpXZ80Q);
			((Control)panelCats).get_Controls().Add((Control)(object)grpTycho2);
			((Control)panelCats).get_Controls().Add((Control)(object)grpNomad);
			((Control)panelCats).set_Location(new Point(4, 25));
			((Control)panelCats).set_Name("panelCats");
			((Control)panelCats).set_Size(new Size(1128, 113));
			((Control)panelCats).set_TabIndex(1);
			((Control)grpGaia).get_Controls().Add((Control)(object)lblGaiaCat);
			((Control)grpGaia).get_Controls().Add((Control)(object)GoogleSky_TychoGaia);
			((Control)grpGaia).get_Controls().Add((Control)(object)label11);
			((Control)grpGaia).get_Controls().Add((Control)(object)cmdGaia);
			((Control)grpGaia).get_Controls().Add((Control)(object)cmbGaia_Dec);
			((Control)grpGaia).get_Controls().Add((Control)(object)cmbGaia_RA);
			((Control)grpGaia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpGaia).set_Location(new Point(125, 2));
			((Control)grpGaia).set_Name("grpGaia");
			((Control)grpGaia).set_Size(new Size(143, 90));
			((Control)grpGaia).set_TabIndex(9);
			grpGaia.set_TabStop(false);
			((Control)grpGaia).set_Text("Gaia");
			((Control)lblGaiaCat).set_AutoSize(true);
			((Control)lblGaiaCat).set_BackColor(Color.Firebrick);
			((Control)lblGaiaCat).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblGaiaCat).set_ForeColor(Color.Yellow);
			((Control)lblGaiaCat).set_Location(new Point(9, 48));
			((Control)lblGaiaCat).set_Name("lblGaiaCat");
			((Control)lblGaiaCat).set_Size(new Size(117, 13));
			((Control)lblGaiaCat).set_TabIndex(5);
			((Control)lblGaiaCat).set_Text("Click to select Gaia Cat");
			((Control)lblGaiaCat).add_Click((EventHandler)lblGaiaCat_Click);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(8, 11));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(124, 13));
			((Control)label11).set_TabIndex(0);
			((Control)label11).set_Text("R.A. step         Dec zone");
			((Control)cmdGaia).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdGaia).set_Location(new Point(16, 62));
			((Control)cmdGaia).set_Name("cmdGaia");
			((Control)cmdGaia).set_Size(new Size(53, 25));
			((Control)cmdGaia).set_TabIndex(3);
			((Control)cmdGaia).set_Text("Display");
			((ButtonBase)cmdGaia).set_UseVisualStyleBackColor(true);
			((Control)cmdGaia).add_Click((EventHandler)cmdGaia_Click);
			cmbGaia_Dec.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbGaia_Dec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbGaia_Dec).set_FormattingEnabled(true);
			((Control)cmbGaia_Dec).set_Location(new Point(80, 26));
			cmbGaia_Dec.set_MaxDropDownItems(30);
			((Control)cmbGaia_Dec).set_Name("cmbGaia_Dec");
			((Control)cmbGaia_Dec).set_Size(new Size(56, 21));
			((Control)cmbGaia_Dec).set_TabIndex(2);
			cmbGaia_RA.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbGaia_RA).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbGaia_RA).set_FormattingEnabled(true);
			((Control)cmbGaia_RA).set_Location(new Point(8, 26));
			cmbGaia_RA.set_MaxDropDownItems(30);
			((Control)cmbGaia_RA).set_Name("cmbGaia_RA");
			((Control)cmbGaia_RA).set_Size(new Size(68, 21));
			((Control)cmbGaia_RA).set_TabIndex(1);
			((Control)label10).set_Anchor((AnchorStyles)1);
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Location(new Point(367, 137));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(401, 13));
			((Control)label10).set_TabIndex(3);
			((Control)label10).set_Text("Right click on an entry to copy, do a catalogue comparison, or display in GoogleSky");
			((ListControl)lstGaiaFiles).set_FormattingEnabled(true);
			((Control)lstGaiaFiles).set_Location(new Point(117, 137));
			((Control)lstGaiaFiles).set_Name("lstGaiaFiles");
			((Control)lstGaiaFiles).set_Size(new Size(153, 186));
			((Control)lstGaiaFiles).set_TabIndex(17);
			((Control)lstGaiaFiles).set_Visible(false);
			lstGaiaFiles.add_Click((EventHandler)lstGaiaFiles_Click);
			lstGaiaFiles.add_SelectedIndexChanged((EventHandler)lstGaiaFiles_SelectedIndexChanged);
			((Control)lstGaiaFiles).add_DoubleClick((EventHandler)lstGaiaFiles_DoubleClick);
			((Control)lstGaiaFiles).add_Leave((EventHandler)lstGaiaFiles_Leave);
			((Control)lblColumns).set_AutoSize(true);
			((Control)lblColumns).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblColumns).set_Location(new Point(1, 150));
			((Control)lblColumns).set_Name("lblColumns");
			((Control)lblColumns).set_Size(new Size(56, 14));
			((Control)lblColumns).set_TabIndex(18);
			((Control)lblColumns).set_Text("Columns");
			((Control)lblUnits).set_AutoSize(true);
			((Control)lblUnits).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)lblUnits).set_Location(new Point(1, 165));
			((Control)lblUnits).set_Name("lblUnits");
			((Control)lblUnits).set_Size(new Size(42, 14));
			((Control)lblUnits).set_TabIndex(19);
			((Control)lblUnits).set_Text("Units");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(1135, 623));
			((Control)this).get_Controls().Add((Control)(object)lstGaiaFiles);
			((Control)this).get_Controls().Add((Control)(object)panelCats);
			((Control)this).get_Controls().Add((Control)(object)lstStars);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)label10);
			((Control)this).get_Controls().Add((Control)(object)lblColumns);
			((Control)this).get_Controls().Add((Control)(object)lblUnits);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationStarsDisplayRanges", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationStarsDisplayRanges);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(200, 200));
			((Control)this).set_Name("Star_DisplayRange");
			((Control)this).set_Text("Display a range of stars from various catalogues");
			((Form)this).add_Load((EventHandler)Star_DisplayRange_Load);
			((Control)this).add_Resize((EventHandler)Star_DisplayRange_Resize);
			((Control)contextMenuStrip1).ResumeLayout(false);
			((Control)grpNomad).ResumeLayout(false);
			((Control)grpNomad).PerformLayout();
			((Control)grpTycho2).ResumeLayout(false);
			((Control)grpTycho2).PerformLayout();
			((Control)grpUser).ResumeLayout(false);
			((Control)grpUser).PerformLayout();
			((Control)grpXZ80Q).ResumeLayout(false);
			((Control)grpXZ80Q).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)grpUCAC4).ResumeLayout(false);
			((Control)grpUCAC4).PerformLayout();
			((Control)grpPPMXL).ResumeLayout(false);
			((Control)grpPPMXL).PerformLayout();
			((Control)panelCats).ResumeLayout(false);
			((Control)grpGaia).ResumeLayout(false);
			((Control)grpGaia).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
