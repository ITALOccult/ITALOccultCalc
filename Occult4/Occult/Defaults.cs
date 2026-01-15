using System;
using System.ComponentModel;
using System.Configuration;
using System.Diagnostics;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;
using Occult.Star_Catalogues;

namespace Occult
{
	public class Defaults : Form
	{
		private readonly string AppPath;

		private IContainer components;

		private GroupBox grpHomeLocation;

		private Label label2;

		private Label label1;

		private Label label4;

		private Label label3;

		private GroupBox grpURLs;

		private Label label9;

		private TextBox txtFutureAllFile;

		private TextBox txtFutureAllServer;

		private Label label8;

		private TextBox txtFutureFile;

		private TextBox txtFutureServer;

		private Label label7;

		private TextBox txtCometFile;

		private TextBox txtCometServer;

		private Label label6;

		private TextBox txtMPCOrbFile;

		private TextBox txtMPCOrbServer;

		private Label label5;

		private TextBox txtAstorbFile;

		private TextBox txtAstorbServer;

		private Label label10;

		public TextBox FTPPassword;

		private Label label12;

		private NumericUpDown updnUncertainty;

		private NumericUpDown numericUpDown9;

		private NumericUpDown numericUpDown10;

		private NumericUpDown numericUpDown11;

		private NumericUpDown numericUpDown12;

		private NumericUpDown numericUpDown5;

		private NumericUpDown numericUpDown6;

		private NumericUpDown numericUpDown7;

		private NumericUpDown numericUpDown8;

		private NumericUpDown numericUpDown1;

		private NumericUpDown numericUpDown2;

		private NumericUpDown numericUpDown3;

		private NumericUpDown numericUpDown4;

		private Label label13;

		private Label label14;

		private Label label15;

		private Label label16;

		private NumericUpDown updnSouth;

		private NumericUpDown updnNorth;

		private NumericUpDown updnEast;

		private NumericUpDown updnWest;

		private Label label20;

		private Label label19;

		private Label label18;

		private Label label17;

		private GroupBox grpStarChartInitialise;

		private ToolStripMenuItem x1DegToolStripMenuItem;

		private ToolStripMenuItem x2DegToolStripMenuItem;

		private ToolStripMenuItem x3DegToolStripMenuItem;

		private ToolStripMenuItem toolStripMenuItem11;

		private ToolStripMenuItem x5DegToolStripMenuItem;

		private ToolStripMenuItem x6DegToolStripMenuItem;

		private ToolStripMenuItem x8DegToolStripMenuItem;

		private ToolStripMenuItem x10DegToolStripMenuItem;

		private ToolStripMenuItem x12DegToolStripMenuItem;

		private ToolStripMenuItem x15DegToolStripMenuItem;

		private ToolStripMenuItem x20DegToolStripMenuItem;

		private ComboBox cmbMag;

		private ComboBox cmbSize;

		private ToolStripMenuItem toolStripMenuItem1;

		private ToolStripMenuItem toolStripMenuItem2;

		private ToolStripMenuItem toolStripMenuItem3;

		private ToolStripMenuItem toolStripMenuItem4;

		private ToolStripMenuItem toolStripMenuItem5;

		private ToolStripMenuItem toolStripMenuItem6;

		private ToolStripMenuItem toolStripMenuItem7;

		private ToolStripMenuItem toolStripMenuItem8;

		private ToolStripMenuItem toolStripMenuItem9;

		private ToolStripMenuItem toolStripMenuItem10;

		private ToolStripMenuItem toolStripMenuItem12;

		private Label label22;

		private Label label21;

		private Panel panel1;

		private NumericUpDown updnDAteRangeInAsteroidASelection;

		private Label label23;

		private Label label24;

		private TextBox txtVizierAstorbFile;

		private TextBox txtVizierAstorbServer;

		private TextBox txtCZMirrorFile;

		private TextBox txtCZMirrorServer;

		private Label label25;

		private CheckBox chkAutoGenerate;

		private CheckBox chkExportElements;

		private CheckBox chkWorld;

		private CheckBox chkHTM;

		private CheckBox chkGoogleEarth;

		private CheckBox chkPrepoint;

		private CheckBox chkMultisite;

		private CheckBox chkPathCoords;

		private CheckBox chkStarChart;

		private ComboBox cmbGraphicSaveType;

		private Label label35;

		private Label label34;

		private Label label33;

		private Label label32;

		private Label label31;

		private Label label30;

		private Label label29;

		private Label label28;

		private Label label27;

		private NumericUpDown updnUranus;

		private NumericUpDown updnMars;

		private NumericUpDown updnJupiter;

		private NumericUpDown updnNeptune;

		private NumericUpDown updnPluto;

		private NumericUpDown updnSaturn;

		private NumericUpDown updnVenus;

		private NumericUpDown updnMercury;

		private ComboBox cmbSiteFiles;

		private Label label41;

		private Label label43;

		private NumericUpDown updnGraze45;

		private Label label42;

		private NumericUpDown updnGraze65;

		private CheckBox chkGrazeProfileKM_Mi;

		private Panel panel3;

		private Label label44;

		private RadioButton optGrazeFormatDMS;

		private RadioButton optGrazeFormatDMM;

		private GroupBox grpLunarOccultations;

		private Label label45;

		private Label label46;

		private NumericUpDown updnAlt;

		private NumericUpDown updnStep;

		private Label label47;

		private Label label48;

		private NumericUpDown updnEndLong;

		private NumericUpDown updnStartLong;

		private Label label49;

		private ComboBox cmbLibrations;

		private Label label50;

		private CheckBox checkBox1;

		private ComboBox cmbGrazeProfile_FileType;

		private Label label51;

		private NumericUpDown numericUpDown29;

		private NumericUpDown numericUpDown30;

		private NumericUpDown numericUpDown31;

		private NumericUpDown numericUpDown32;

		private TextBox textBox34;

		private NumericUpDown numericUpDown25;

		private NumericUpDown numericUpDown26;

		private NumericUpDown numericUpDown27;

		private NumericUpDown numericUpDown28;

		private TextBox textBox33;

		private NumericUpDown numericUpDown21;

		private NumericUpDown numericUpDown22;

		private NumericUpDown numericUpDown23;

		private NumericUpDown numericUpDown24;

		private TextBox textBox32;

		private NumericUpDown numericUpDown17;

		private NumericUpDown numericUpDown18;

		private NumericUpDown numericUpDown19;

		private NumericUpDown numericUpDown20;

		private TextBox textBox31;

		private NumericUpDown numericUpDown16;

		private NumericUpDown numericUpDown15;

		private NumericUpDown numericUpDown14;

		private NumericUpDown numericUpDown13;

		private TextBox textBox30;

		private NumericUpDown numericUpDown37;

		private NumericUpDown numericUpDown38;

		private NumericUpDown numericUpDown39;

		private NumericUpDown numericUpDown40;

		private TextBox textBox36;

		private NumericUpDown numericUpDown33;

		private NumericUpDown numericUpDown34;

		private NumericUpDown numericUpDown35;

		private NumericUpDown numericUpDown36;

		private TextBox textBox35;

		private Label label52;

		private Label label56;

		private Label label55;

		private Label label54;

		private Label label53;

		private Label label57;

		private NumericUpDown updnJupiterDiameter;

		private NumericUpDown updnSaturnDiameter;

		private NumericUpDown updnUranusDiameter;

		private NumericUpDown updnPlutoDiameter;

		private NumericUpDown updnTitanDiameter;

		private NumericUpDown updnTritonDiameter;

		private NumericUpDown updnNeptuneDiameter;

		private Label label63;

		private Label label62;

		private Label label61;

		private Label label60;

		private Label label59;

		private Label label58;

		private Label label64;

		private TextBox txtNOMAD;

		private GroupBox grpOptionalCatalogues;

		private ToolTip toolTip1;

		private CheckBox chkNomad;

		private Label label66;

		private TextBox txtNomadShort;

		private Label label67;

		private Label label72;

		private Label label73;

		private Label label74;

		private Label label75;

		private Label label76;

		private GroupBox grpGoogleEarth;

		private GroupBox grpAsteroidSearchDisplay;

		private Label label77;

		private Label label78;

		private GroupBox grpFileSaveGraphics;

		private Label label80;

		private Label label26;

		private GroupBox grpSiteFiles;

		private Label label81;

		private Label label40;

		private ComboBox cmbSortOrder;

		private ComboBox cmbSitePlot;

		private Label label39;

		private NumericUpDown updnMagCorrection;

		private Label label38;

		private Label label37;

		private NumericUpDown updnTelescopeAperture;

		private Label label36;

		private NumericUpDown updnGrazeTtravelDistance;

		private Label label83;

		private Label label84;

		private Label label85;

		private NumericUpDown numericUpDown56;

		private NumericUpDown numericUpDown55;

		private NumericUpDown numericUpDown54;

		private NumericUpDown numericUpDown53;

		private NumericUpDown numericUpDown52;

		private NumericUpDown numericUpDown51;

		private NumericUpDown numericUpDown50;

		private NumericUpDown numericUpDown49;

		private NumericUpDown numericUpDown48;

		private NumericUpDown numericUpDown47;

		private NumericUpDown numericUpDown46;

		private NumericUpDown numericUpDown45;

		private NumericUpDown numericUpDown44;

		private NumericUpDown numericUpDown43;

		private NumericUpDown numericUpDown42;

		private NumericUpDown numericUpDown41;

		private Label label90;

		private Label label91;

		private Label label92;

		private Label label93;

		private Label label86;

		private Label label87;

		private Label label88;

		private Label label89;

		private Label label79;

		private Label label95;

		private Label label94;

		private Label label96;

		private GroupBox grpAdministrator;

		private Label label97;

		private CheckBox checkBox2;

		private GroupBox grpLunarReports;

		private Label label68;

		private NumericUpDown updnCentury;

		private Label label69;

		private Label label70;

		private Label label71;

		private CheckBox checkBox3;

		private Label label105;

		private GroupBox grpAsteroidMultiPathMaps;

		private CheckBox chkColour;

		private CheckBox chkBessel;

		private RadioButton optGrazeFormatDDD;

		private Label label107;

		private Label label108;

		private ComboBox cmbAutoFileType;

		private Label label110;

		private Label label111;

		private Label label109;

		private CheckBox chkAutoNomad;

		private ComboBox cmbAutoSizeDeg;

		private ComboBox cmbAutoMagLimit;

		private ComboBox cmbAutoSizePixels;

		private Label label112;

		private Label label113;

		private Label label114;

		private CheckBox chkSmoothing;

		private CheckBox chkPreserveFutureDAT;

		private Label label115;

		private Label label116;

		private Label label118;

		private TextBox textBox4;

		private Label label119;

		private Label label120;

		private ComboBox cmbLibrationsReduce;

		private TextBox textBox5;

		private Label label123;

		private Label label122;

		private Label label121;

		private TextBox textBox6;

		private CheckBox chkLunarGlobal;

		private CheckBox chkLunarRegional;

		private CheckBox checkBox5;

		private TextBox txtPort;

		private Label label124;

		private GroupBox grpEmailAdvanced;

		private Label label125;

		private Button cmdHelpInternet;

		private Label label128;

		private NumericUpDown numericUpDown57;

		private Label label127;

		private Label label126;

		private CheckBox chkMultiSiteAltitude;

		private Label label129;

		private NumericUpDown updnLineThickness;

		private Label label130;

		private NumericUpDown updnFontSize;

		private Label label131;

		private Label label132;

		private NumericUpDown updnEOPReminder;

		private CheckBox checkBox6;

		private OpenFileDialog openFileDialog1;

		private Button cmdFolderNOMAD;

		private Button cmdFolderNomad_Short;

		private TextBox txtIFfile;

		private TextBox txtWDSfile;

		private TextBox txtIFserver;

		private TextBox txtWDSserver;

		private Label label136;

		private Label label135;

		private Label label134;

		private Label label138;

		private Label label137;

		private Label label139;

		private TextBox txtVSXfile;

		private TextBox txtVSXserver;

		private Label label140;

		private Label label141;

		private Label label143;

		private Label label142;

		private TextBox txtAstDysServer;

		private TextBox txtAstDysFile;

		private GroupBox grpUpdates;

		private ComboBox cmbUpdateFrequency;

		private Label label144;

		private Label label145;

		private Label label147;

		private Button cmdResetDownloadAddresses;

		private TextBox txtEOPpre62_File;

		private TextBox txtEOPpost62_File;

		private TextBox txtEOPpre62_Server;

		private TextBox txtEOPpost62_Server;

		private Label label148;

		private Label label150;

		private Label label149;

		private Label label151;

		private TextBox txtOccultServer;

		private TextBox txtCALL;

		private Label label153;

		private Label label155;

		private GroupBox grpTest;

		private CheckBox chkForceVSOP87;

		private Button cmdPurgeLunarCache;

		private Label label156;

		private TextBox txtOrb6File;

		private TextBox txtOrb6Server;

		private CheckBox chkLunarCoordinates;

		private TextBox textBox2;

		private Label label158;

		private NumericUpDown numericUpDown58;

		private Label label159;

		private Label label157;

		private NumericUpDown numericUpDown59;

		private Button cmdFolder_UCAC4;

		private Label label160;

		private TextBox txtUCAC4;

		private CheckBox chkUCAC4;

		private CheckBox chkAutoUCAC4;

		private CheckBox chkAutoPPMXL;

		private CheckBox chkPPMXL;

		private CheckBox chkReposition;

		private NumericUpDown updnPlutoDecCorrn;

		private NumericUpDown updnPlutoRACorrn;

		private CheckBox chkAddSAO;

		private Button cmdU4XRefs;

		private Label label162;

		private Button cmdU4Help;

		private PictureBox pictureBox1;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem undoSessionChangesToolStripMenuItem;

		private ToolStripMenuItem resstToDefaultsToolStripMenuItem;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem saveAndExitToolStripMenuItem;

		private Label label106;

		private Label label82;

		private Label label163;

		private Button cmdUCAC4CheckCorrected;

		internal TextBox txtLongDDD;

		internal TextBox txtLatDDD;

		internal TextBox txtTimeZone;

		internal TextBox txtAlt;

		internal TextBox txtSiteName;

		private TextBox txtRIO_server;

		private Label label164;

		private TextBox txtWDScodesFile;

		private TextBox txtWDScodesServer;

		private Label label166;

		private TextBox txt7Timer;

		private PictureBox pictureBox2;

		private Label label168;

		private NumericUpDown updnMaxSep;

		private NumericUpDown updnMinSep;

		private Label label170;

		private Label label169;

		private Label label171;

		private Label label172;

		private ComboBox cmbLightCurveReminder;

		private Label label173;

		private Label label174;

		private CheckBox chkGaia;

		private CheckBox chkGaia12;

		private Label label117;

		private Label label175;

		private CheckBox checkBox8;

		private CheckBox checkBox4;

		private GroupBox grpEmails;

		private GroupBox grpC2A;

		private Label label178;

		private Button cmdFolder_C2A;

		private TextBox txtC2A;

		private Label label177;

		private LinkLabel linkC2A;

		private ComboBox cmbC2AFieldSize;

		private Label label146;

		private Label label179;

		private RadioButton optUCAC4;

		private RadioButton optPPMXL;

		private RadioButton optNOMAD;

		private RadioButton optSAO;

		private CheckBox chkC2A_MultipleInstances;

		private PictureBox pictureBox3;

		private TextBox txtLuckyStarServer;

		private Label label180;

		private Label label181;

		private NumericUpDown updnMaximumSearchEvents;

		private Label label182;

		private Panel panel2;

		private Label label183;

		private Button cmdDownload_u4i_files;

		private Label label184;

		private TextBox txtJupSats_Server;

		private Label label185;

		internal TextBox txtAperture;

		private Label label11;

		private Panel panel4;

		private Label label65;

		private CheckBox chkGaia16Auto;

		private CheckBox checkBox7;

		private Label label133;

		private TextBox txtGoogleMapsAPI;

		public Defaults()
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			InitializeComponent();
			AppPath = Utilities.AppPath;
		}

		private void Defaults_Load(object sender, EventArgs e)
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			((Control)panel1).set_Left(0);
			((Control)panel1).set_Top(24);
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
			if (((Control)this).get_Top() < -30000)
			{
				((Control)this).set_Top(0);
			}
			if ((double)((Control)this).get_Left() + (double)((Control)this).get_Width() / 1.2 < (double)Screen.GetWorkingArea((Control)(object)this).Left)
			{
				((Control)this).set_Left(Screen.GetWorkingArea((Control)(object)this).Left);
			}
			((Control)this).set_Height((int)((double)Screen.GetWorkingArea((Control)(object)this).Height * 0.95));
			((Control)grpUpdates).set_Top(((Control)grpHomeLocation).get_Bottom() + 40);
			((Control)grpSiteFiles).set_Top(((Control)grpUpdates).get_Bottom() + 40);
			((Control)grpEmails).set_Top(((Control)grpSiteFiles).get_Bottom() + 40);
			((Control)grpURLs).set_Top(((Control)grpEmails).get_Bottom() + 40);
			((Control)grpGoogleEarth).set_Top(((Control)grpURLs).get_Bottom() + 40);
			((Control)grpOptionalCatalogues).set_Top(((Control)grpGoogleEarth).get_Bottom() + 40);
			((Control)grpFileSaveGraphics).set_Top(((Control)grpOptionalCatalogues).get_Bottom() + 40);
			((Control)grpStarChartInitialise).set_Top(((Control)grpFileSaveGraphics).get_Bottom() + 40);
			((Control)grpC2A).set_Top(((Control)grpStarChartInitialise).get_Bottom() + 40);
			((Control)grpAsteroidSearchDisplay).set_Top(((Control)grpC2A).get_Bottom() + 40);
			((Control)grpAsteroidMultiPathMaps).set_Top(((Control)grpAsteroidSearchDisplay).get_Bottom() + 40);
			((Control)grpLunarOccultations).set_Top(((Control)grpAsteroidMultiPathMaps).get_Bottom() + 40);
			((Control)grpLunarReports).set_Top(((Control)grpLunarOccultations).get_Bottom() + 40);
			((Control)grpAdministrator).set_Top(((Control)grpLunarReports).get_Bottom() + 40);
			((Control)grpTest).set_Top(((Control)grpAdministrator).get_Bottom() + 40);
			((ListControl)cmbUpdateFrequency).set_SelectedIndex(Settings.Default.UpdateFrequency);
			((ListControl)cmbLightCurveReminder).set_SelectedIndex(Settings.Default.LightCurveReminder);
			((ListControl)cmbSize).set_SelectedIndex(Settings.Default.StarChartSizeIndex);
			((ListControl)cmbMag).set_SelectedIndex(Settings.Default.StarChartMagLimitIndex - 6);
			((ListControl)cmbGraphicSaveType).set_SelectedIndex(Settings.Default.GraphicsSaveFileType);
			((ListControl)cmbSitePlot).set_SelectedIndex(Settings.Default.Site_PlotControl);
			((ListControl)cmbSortOrder).set_SelectedIndex(Settings.Default.Sites_EditorSortOrder);
			((ListControl)cmbLibrations).set_SelectedIndex(Settings.Default.GrazeProfile_LibrationPlot);
			((ListControl)cmbLibrationsReduce).set_SelectedIndex(Settings.Default.GrazeProfile_LibrationPlot_Reduction);
			((ListControl)cmbGrazeProfile_FileType).set_SelectedIndex(Settings.Default.GrazeProfile_FileFormat);
			((ListControl)cmbAutoFileType).set_SelectedIndex(Settings.Default.GraphicsAutoSaveFileType);
			((ListControl)cmbAutoSizePixels).set_SelectedIndex(Settings.Default.AutoGenerateStarWidthPixels);
			((ListControl)cmbAutoSizeDeg).set_SelectedIndex(Settings.Default.AutoGenerateStarWidthDeg);
			((ListControl)cmbAutoMagLimit).set_SelectedIndex(Settings.Default.AutoGenerateStarMag);
			string[] files = Directory.GetFiles(AppPath + "\\Sites", "*.site");
			foreach (string path in files)
			{
				cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
			}
			cmbSiteFiles.get_Items().Add((object)"   don't plot");
			string siteFileForAutoMap = Settings.Default.SiteFileForAutoMap;
			((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			for (int j = 0; j < cmbSiteFiles.get_Items().get_Count(); j++)
			{
				if (siteFileForAutoMap == cmbSiteFiles.get_Items().get_Item(j).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(j);
					break;
				}
			}
			chkForceVSOP87.set_Checked(Utilities.MustUseVSOP87);
			linkC2A.get_Links().Add(123, 10, (object)"http://www.astrosurf.com/c2a/english/download.htm");
			if (Settings.Default.C2AFieldSize == "")
			{
				((ListControl)cmbC2AFieldSize).set_SelectedIndex(3);
			}
			else
			{
				for (int k = 0; k < cmbC2AFieldSize.get_Items().get_Count(); k++)
				{
					if (cmbC2AFieldSize.get_Items().get_Item(k).ToString() == Settings.Default.C2AFieldSize)
					{
						((ListControl)cmbC2AFieldSize).set_SelectedIndex(k);
						break;
					}
				}
			}
			SetC2ACatalog();
			Cursor.set_Current(Cursors.get_Default());
		}

		public static void SaveDefaults()
		{
			((SettingsBase)Settings.Default).Save();
			GetStarPosition.CheckStarCatsPresent();
		}

		private void cmbUpdateFrequency_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.UpdateFrequency = ((ListControl)cmbUpdateFrequency).get_SelectedIndex();
		}

		private void cmbLightCurveReminder_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.LightCurveReminder = ((ListControl)cmbLightCurveReminder).get_SelectedIndex();
		}

		private void cmbSize_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.StarChartSizeIndex = ((ListControl)cmbSize).get_SelectedIndex();
		}

		private void cmbMag_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.StarChartMagLimitIndex = ((ListControl)cmbMag).get_SelectedIndex() + 6;
		}

		private void cmbGraphicSaveType_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.GraphicsSaveFileType = ((ListControl)cmbGraphicSaveType).get_SelectedIndex();
		}

		private void chkGoogleEarth_CheckedChanged(object sender, EventArgs e)
		{
			if (chkGoogleEarth.get_Checked())
			{
				chkPathCoords.set_Checked(true);
			}
		}

		private void chkHTM_CheckedChanged(object sender, EventArgs e)
		{
			if (chkHTM.get_Checked())
			{
				chkPathCoords.set_Checked(true);
			}
		}

		private void chkPathCoords_CheckedChanged(object sender, EventArgs e)
		{
			if (!chkPathCoords.get_Checked())
			{
				CheckBox obj = chkGoogleEarth;
				bool @checked;
				chkHTM.set_Checked(@checked = false);
				obj.set_Checked(@checked);
			}
		}

		private void cmbSitePlot_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.Site_PlotControl = ((ListControl)cmbSitePlot).get_SelectedIndex();
		}

		private void Defaults_FormClosing(object sender, FormClosingEventArgs e)
		{
			SaveDefaults();
		}

		private void cmbSortOrder_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.Sites_EditorSortOrder = ((ListControl)cmbSortOrder).get_SelectedIndex();
		}

		private void cmbSiteFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.SiteFileForAutoMap = cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString();
		}

		private void cmbLibrations_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.GrazeProfile_LibrationPlot = ((ListControl)cmbLibrations).get_SelectedIndex();
		}

		private void cmbLibrationsReduce_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.GrazeProfile_LibrationPlot_Reduction = ((ListControl)cmbLibrationsReduce).get_SelectedIndex();
		}

		private void cmbFileType_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.GrazeProfile_FileFormat = ((ListControl)cmbGrazeProfile_FileType).get_SelectedIndex();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
		}

		private void txtLongDDD_TextChanged(object sender, EventArgs e)
		{
			if (double.TryParse(((Control)txtLongDDD).get_Text(), out var result))
			{
				while (result > 180.0)
				{
					result -= 360.0;
				}
				for (; result < -180.0; result += 360.0)
				{
				}
				if (result < -135.0 || result > 135.0)
				{
					Settings.Default.StartHour = 0;
				}
				else if (result < -45.0)
				{
					Settings.Default.StartHour = -6;
				}
				else if (result < 45.0)
				{
					Settings.Default.StartHour = 12;
				}
				else if (result < 135.0)
				{
					Settings.Default.StartHour = 6;
				}
			}
		}

		private void txtLongDDD_Leave(object sender, EventArgs e)
		{
			if (((Control)txtLongDDD).get_Text().Contains(","))
			{
				((Control)txtLongDDD).set_Text(((Control)txtLongDDD).get_Text().Replace(",", "."));
			}
		}

		private void txtLatDDD_Leave(object sender, EventArgs e)
		{
			if (((Control)txtLatDDD).get_Text().Contains(","))
			{
				((Control)txtLatDDD).set_Text(((Control)txtLatDDD).get_Text().Replace(",", "."));
			}
		}

		private void textBox30_Leave(object sender, EventArgs e)
		{
			if (Verify(((Control)textBox30).get_Text()))
			{
				((Control)textBox30).Focus();
			}
		}

		private void textBox32_Leave(object sender, EventArgs e)
		{
			if (Verify(((Control)textBox32).get_Text()))
			{
				((Control)textBox32).Focus();
			}
		}

		private void textBox31_Leave(object sender, EventArgs e)
		{
			if (Verify(((Control)textBox31).get_Text()))
			{
				((Control)textBox31).Focus();
			}
		}

		private void textBox33_Leave(object sender, EventArgs e)
		{
			if (Verify(((Control)textBox33).get_Text()))
			{
				((Control)textBox33).Focus();
			}
		}

		private void textBox34_Leave(object sender, EventArgs e)
		{
			if (Verify(((Control)textBox34).get_Text()))
			{
				((Control)textBox34).Focus();
			}
		}

		private void textBox35_Leave(object sender, EventArgs e)
		{
			if (Verify(((Control)textBox35).get_Text()))
			{
				((Control)textBox35).Focus();
			}
		}

		private void textBox36_Leave(object sender, EventArgs e)
		{
			if (Verify(((Control)textBox36).get_Text()))
			{
				((Control)textBox36).Focus();
			}
		}

		private bool Verify(string Name)
		{
			//IL_0121: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			if (Name.Contains("/"))
			{
				text += "/ ";
			}
			if (Name.Contains("\\"))
			{
				text += "\\ ";
			}
			if (Name.Contains(":"))
			{
				text += ": ";
			}
			if (Name.Contains("*"))
			{
				text += "* ";
			}
			if (Name.Contains("#"))
			{
				text += "# ";
			}
			if (Name.Contains("?"))
			{
				text += "? ";
			}
			if (Name.Contains("\""))
			{
				text += "\" ";
			}
			if (Name.Contains("<"))
			{
				text += "< ";
			}
			if (Name.Contains(">"))
			{
				text += "> ";
			}
			if (Name.Contains("|"))
			{
				text += "| ";
			}
			if (text.Length > 0)
			{
				MessageBox.Show("Region name contains the invalid character(s) " + text + "\r\nThese characters must be removed", "Invalid character", (MessageBoxButtons)0, (MessageBoxIcon)16);
			}
			return text.Length > 0;
		}

		private void cmbAutoFileType_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.GraphicsAutoSaveFileType = ((ListControl)cmbAutoFileType).get_SelectedIndex();
		}

		private void cmbAutoSizePixels_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.AutoGenerateStarWidthPixels = ((ListControl)cmbAutoSizePixels).get_SelectedIndex();
		}

		private void cmbAutoSizeDeg_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.AutoGenerateStarWidthDeg = ((ListControl)cmbAutoSizeDeg).get_SelectedIndex();
		}

		private void cmbAutoMagLimit_SelectedIndexChanged(object sender, EventArgs e)
		{
			Settings.Default.AutoGenerateStarMag = ((ListControl)cmbAutoMagLimit).get_SelectedIndex();
		}

		private void cmdHelpInternet_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Email settings");
		}

		private void cmdFolder_UCAC4_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0030: Invalid comparison between Unknown and I4
			FolderBrowserDialog val = new FolderBrowserDialog();
			val.set_ShowNewFolderButton(false);
			val.set_SelectedPath(((Control)txtUCAC4).get_Text());
			val.set_Description("Select folder containing UCAC4 files");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				((Control)txtUCAC4).set_Text(val.get_SelectedPath());
			}
		}

		private void cmdU4HipLookup_Click(object sender, EventArgs e)
		{
			UCAC4.CreateHipXrefFile();
			UCAC4.Create_U4Tycho2_xref_files();
		}

		private void cmdUCAC4CheckCorrected_Click(object sender, EventArgs e)
		{
			UCAC4.CheckUCAC4_Updated(CheckAll: true);
		}

		private void cmdFolderNomad_Short_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0030: Invalid comparison between Unknown and I4
			FolderBrowserDialog val = new FolderBrowserDialog();
			val.set_ShowNewFolderButton(false);
			val.set_SelectedPath(((Control)txtNomadShort).get_Text());
			val.set_Description("Select folder containing NOMAD_Short files");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				((Control)txtNomadShort).set_Text(val.get_SelectedPath());
			}
		}

		private void cmdFolderNOMAD_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0030: Invalid comparison between Unknown and I4
			FolderBrowserDialog val = new FolderBrowserDialog();
			val.set_ShowNewFolderButton(false);
			val.set_SelectedPath(((Control)txtNOMAD).get_Text());
			val.set_Description("Select folder containing NOMAD files");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				((Control)txtNOMAD).set_Text(val.get_SelectedPath());
			}
		}

		private void cmdFolder_C2A_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_002a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0030: Invalid comparison between Unknown and I4
			FolderBrowserDialog val = new FolderBrowserDialog();
			val.set_ShowNewFolderButton(false);
			val.set_SelectedPath(((Control)txtC2A).get_Text());
			val.set_Description("Select the folder where you have installed C2A");
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				((Control)txtC2A).set_Text(val.get_SelectedPath());
			}
		}

		private void txtAstorbServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.ASTORB_Server = ((Control)txtAstorbServer).get_Text().Trim();
		}

		private void txtAstorbFile_Leave(object sender, EventArgs e)
		{
			Settings.Default.ASTORB_file = ((Control)txtAstorbFile).get_Text().Trim();
		}

		private void txtMPCOrbServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.MPCOrb_Server = ((Control)txtMPCOrbServer).get_Text().Trim();
		}

		private void txtMPCOrbFile_Leave(object sender, EventArgs e)
		{
			Settings.Default.MPCOrb_file = ((Control)txtMPCOrbFile).get_Text().Trim();
		}

		private void txtCZMirrorServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.MPCORB_Mirror_Server = ((Control)txtCZMirrorServer).get_Text().Trim();
		}

		private void txtCZMirrorFile_TextChanged(object sender, EventArgs e)
		{
			Settings.Default.MPCOrb_Mirror_file = ((Control)txtCZMirrorFile).get_Text().Trim();
		}

		private void txtVizierAstorbServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.ASTORB_Mirror = ((Control)txtVizierAstorbServer).get_Text().Trim();
		}

		private void txtVizierAstorbFile_Leave(object sender, EventArgs e)
		{
			Settings.Default.ASTORB_Mirror_file = ((Control)txtVizierAstorbFile).get_Text().Trim();
		}

		private void txtAstDysServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.AstDys2_Server = ((Control)txtAstDysServer).get_Text().Trim();
		}

		private void txtAstDysFile_Leave(object sender, EventArgs e)
		{
			Settings.Default.AstDys2_AllNumFile = ((Control)txtAstDysFile).get_Text().Trim();
		}

		private void txtCometServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.Comet_Server = ((Control)txtCometServer).get_Text().Trim();
		}

		private void txtCometFile_Leave(object sender, EventArgs e)
		{
			Settings.Default.Comet_file = ((Control)txtCometFile).get_Text().Trim();
		}

		private void txtFutureServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.Future_Server = ((Control)txtFutureServer).get_Text().Trim();
		}

		private void txtFutureFile_Leave(object sender, EventArgs e)
		{
			Settings.Default.FutureFile_xml = ((Control)txtFutureFile).get_Text().Trim();
		}

		private void txtFutureAllServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.FutureAll_Server = ((Control)txtFutureAllServer).get_Text().Trim();
		}

		private void txtFutureAllFile_Leave(object sender, EventArgs e)
		{
			Settings.Default.FutureAll_File_XML = ((Control)txtFutureAllFile).get_Text().Trim();
		}

		private void txtRIO_server_Leave(object sender, EventArgs e)
		{
			Settings.Default.RIO_server = ((Control)txtRIO_server).get_Text().Trim();
		}

		private void txtWDSserver_Leave(object sender, EventArgs e)
		{
			Settings.Default.WDSDownloadServer = ((Control)txtWDSserver).get_Text().Trim();
		}

		private void txtWDSfile_Leave(object sender, EventArgs e)
		{
			Settings.Default.WDSDownload_File = ((Control)txtWDSfile).get_Text().Trim();
		}

		private void txtIFserver_Leave(object sender, EventArgs e)
		{
			Settings.Default.IFdownloadServer = ((Control)txtIFserver).get_Text().Trim();
		}

		private void txtIFfile_Leave(object sender, EventArgs e)
		{
			Settings.Default.IFdownloadFile = ((Control)txtIFfile).get_Text().Trim();
		}

		private void txtVSXserver_Leave(object sender, EventArgs e)
		{
			Settings.Default.AAVSOdownloadServer = ((Control)txtVSXserver).get_Text().Trim();
		}

		private void txtVSXfile_Leave(object sender, EventArgs e)
		{
			Settings.Default.AAVSOdownloadFile = ((Control)txtVSXfile).get_Text().Trim();
		}

		private void txtEOPpost62_Server_Leave(object sender, EventArgs e)
		{
			Settings.Default.EOPpost62_Server = ((Control)txtEOPpost62_Server).get_Text().Trim();
		}

		private void txtEOPpost62_File_Leave(object sender, EventArgs e)
		{
			Settings.Default.EOPpost62_File = ((Control)txtEOPpost62_File).get_Text().Trim();
		}

		private void txtEOPpre62_Server_Leave(object sender, EventArgs e)
		{
			Settings.Default.EOPpre62_Server = ((Control)txtEOPpre62_Server).get_Text().Trim();
		}

		private void txtEOPpre62_File_Leave(object sender, EventArgs e)
		{
			Settings.Default.EOPpre62_File = ((Control)txtEOPpre62_File).get_Text().Trim();
		}

		private void txtOccultServer_Leave(object sender, EventArgs e)
		{
			Settings.Default.OccultServer = ((Control)txtOccultServer).get_Text().Trim();
		}

		private void txtCALL_Leave(object sender, EventArgs e)
		{
			Settings.Default.AsteroidLightCurveData_url = ((Control)txtCALL).get_Text().Trim();
		}

		private void cmdResetDownloadAddresses_Click(object sender, EventArgs e)
		{
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			TextBox obj = txtOccultServer;
			string text2 = (Settings.Default.OccultServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("OccultServer").get_DefaultValue()
				.ToString());
			((Control)obj).set_Text(text2);
			TextBox obj2 = txtAstorbServer;
			text2 = (Settings.Default.ASTORB_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("ASTORB_Server").get_DefaultValue()
				.ToString());
			((Control)obj2).set_Text(text2);
			TextBox obj3 = txtAstorbFile;
			text2 = (Settings.Default.ASTORB_file = ((SettingsBase)Settings.Default).get_Properties().get_Item("ASTORB_file").get_DefaultValue()
				.ToString());
			((Control)obj3).set_Text(text2);
			TextBox obj4 = txtMPCOrbServer;
			text2 = (Settings.Default.MPCOrb_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("MPCOrb_Server").get_DefaultValue()
				.ToString());
			((Control)obj4).set_Text(text2);
			TextBox obj5 = txtMPCOrbFile;
			text2 = (Settings.Default.MPCOrb_file = ((SettingsBase)Settings.Default).get_Properties().get_Item("MPCOrb_file").get_DefaultValue()
				.ToString());
			((Control)obj5).set_Text(text2);
			TextBox obj6 = txtCZMirrorServer;
			text2 = (Settings.Default.MPCORB_Mirror_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("MPCORB_Mirror_Server").get_DefaultValue()
				.ToString());
			((Control)obj6).set_Text(text2);
			TextBox obj7 = txtCZMirrorFile;
			text2 = (Settings.Default.MPCOrb_Mirror_file = ((SettingsBase)Settings.Default).get_Properties().get_Item("MPCOrb_Mirror_file").get_DefaultValue()
				.ToString());
			((Control)obj7).set_Text(text2);
			TextBox obj8 = txtVizierAstorbServer;
			text2 = (Settings.Default.ASTORB_Mirror = ((SettingsBase)Settings.Default).get_Properties().get_Item("ASTORB_Mirror").get_DefaultValue()
				.ToString());
			((Control)obj8).set_Text(text2);
			TextBox obj9 = txtVizierAstorbFile;
			text2 = (Settings.Default.ASTORB_Mirror_file = ((SettingsBase)Settings.Default).get_Properties().get_Item("ASTORB_Mirror_file").get_DefaultValue()
				.ToString());
			((Control)obj9).set_Text(text2);
			TextBox obj10 = txtAstDysServer;
			text2 = (Settings.Default.AstDys2_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("AstDys2_Server").get_DefaultValue()
				.ToString());
			((Control)obj10).set_Text(text2);
			TextBox obj11 = txtAstDysFile;
			text2 = (Settings.Default.AstDys2_AllNumFile = ((SettingsBase)Settings.Default).get_Properties().get_Item("AstDys2_AllNumFile").get_DefaultValue()
				.ToString());
			((Control)obj11).set_Text(text2);
			TextBox obj12 = txtCometServer;
			text2 = (Settings.Default.Comet_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("Comet_Server").get_DefaultValue()
				.ToString());
			((Control)obj12).set_Text(text2);
			TextBox obj13 = txtCometFile;
			text2 = (Settings.Default.Comet_file = ((SettingsBase)Settings.Default).get_Properties().get_Item("Comet_file").get_DefaultValue()
				.ToString());
			((Control)obj13).set_Text(text2);
			TextBox obj14 = txtFutureServer;
			text2 = (Settings.Default.Future_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("Future_Server").get_DefaultValue()
				.ToString());
			((Control)obj14).set_Text(text2);
			TextBox obj15 = txtFutureFile;
			text2 = (Settings.Default.FutureFile_xml = ((SettingsBase)Settings.Default).get_Properties().get_Item("FutureFile_xml").get_DefaultValue()
				.ToString());
			((Control)obj15).set_Text(text2);
			TextBox obj16 = txtFutureAllServer;
			text2 = (Settings.Default.FutureAll_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("FutureAll_Server").get_DefaultValue()
				.ToString());
			((Control)obj16).set_Text(text2);
			TextBox obj17 = txtFutureAllFile;
			text2 = (Settings.Default.FutureAll_File_XML = ((SettingsBase)Settings.Default).get_Properties().get_Item("FutureAll_File_XML").get_DefaultValue()
				.ToString());
			((Control)obj17).set_Text(text2);
			TextBox obj18 = txtRIO_server;
			text2 = (Settings.Default.RIO_server = ((SettingsBase)Settings.Default).get_Properties().get_Item("RIO_server").get_DefaultValue()
				.ToString());
			((Control)obj18).set_Text(text2);
			TextBox obj19 = txtWDSserver;
			text2 = (Settings.Default.WDSDownloadServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("WDSDownloadServer").get_DefaultValue()
				.ToString());
			((Control)obj19).set_Text(text2);
			TextBox obj20 = txtWDSfile;
			text2 = (Settings.Default.WDSDownload_File = ((SettingsBase)Settings.Default).get_Properties().get_Item("WDSDownload_File").get_DefaultValue()
				.ToString());
			((Control)obj20).set_Text(text2);
			Settings @default = Settings.Default;
			((Control)txtIFserver).set_Text(text2 = ((SettingsBase)Settings.Default).get_Properties().get_Item("IFdownloadServer").get_DefaultValue()
				.ToString());
			@default.IFdownloadServer = text2;
			TextBox obj21 = txtIFfile;
			text2 = (Settings.Default.IFdownloadFile = ((SettingsBase)Settings.Default).get_Properties().get_Item("IFdownloadFile").get_DefaultValue()
				.ToString());
			((Control)obj21).set_Text(text2);
			TextBox obj22 = txtVSXserver;
			text2 = (Settings.Default.AAVSOdownloadServer = ((SettingsBase)Settings.Default).get_Properties().get_Item("AAVSOdownloadServer").get_DefaultValue()
				.ToString());
			((Control)obj22).set_Text(text2);
			TextBox obj23 = txtVSXfile;
			text2 = (Settings.Default.AAVSOdownloadFile = ((SettingsBase)Settings.Default).get_Properties().get_Item("AAVSOdownloadFile").get_DefaultValue()
				.ToString());
			((Control)obj23).set_Text(text2);
			TextBox obj24 = txtEOPpost62_Server;
			text2 = (Settings.Default.EOPpost62_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("EOPpost62_Server").get_DefaultValue()
				.ToString());
			((Control)obj24).set_Text(text2);
			TextBox obj25 = txtEOPpost62_File;
			text2 = (Settings.Default.EOPpost62_File = ((SettingsBase)Settings.Default).get_Properties().get_Item("EOPpost62_File").get_DefaultValue()
				.ToString());
			((Control)obj25).set_Text(text2);
			TextBox obj26 = txtEOPpre62_Server;
			text2 = (Settings.Default.EOPpre62_Server = ((SettingsBase)Settings.Default).get_Properties().get_Item("EOPpre62_Server").get_DefaultValue()
				.ToString());
			((Control)obj26).set_Text(text2);
			TextBox obj27 = txtEOPpre62_File;
			text2 = (Settings.Default.EOPpre62_File = ((SettingsBase)Settings.Default).get_Properties().get_Item("EOPpre62_File").get_DefaultValue()
				.ToString());
			((Control)obj27).set_Text(text2);
			TextBox obj28 = txtCALL;
			text2 = (Settings.Default.AsteroidLightCurveData_url = ((SettingsBase)Settings.Default).get_Properties().get_Item("AsteroidLightCurveData_url").get_DefaultValue()
				.ToString());
			((Control)obj28).set_Text(text2);
			TextBox obj29 = txt7Timer;
			text2 = (Settings.Default.SevenTimer = ((SettingsBase)Settings.Default).get_Properties().get_Item("SevenTimer").get_DefaultValue()
				.ToString());
			((Control)obj29).set_Text(text2);
			((Control)this).set_Cursor(Cursors.get_Default());
		}

		private void chkForceVSOP87_CheckedChanged(object sender, EventArgs e)
		{
			Utilities.MustUseVSOP87 = chkForceVSOP87.get_Checked();
		}

		private void cmdPurgeLunarCache_Click(object sender, EventArgs e)
		{
			JPL_DE.PurgeLunarCache();
		}

		private void cmdU4Help_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"UCAC4");
		}

		private void undoSessionChangesToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Any values entered will be lost", "Comfirm loss of data", (MessageBoxButtons)1, (MessageBoxIcon)48) != 2)
			{
				((ApplicationSettingsBase)Settings.Default).Reload();
			}
		}

		private void resstToDefaultsToolStripMenuItem_Click(object sender, EventArgs e)
		{
			//IL_000d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0013: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("All values will be returned to initial default values", "Comfirm loss of data", (MessageBoxButtons)1, (MessageBoxIcon)48) != 2)
			{
				((ApplicationSettingsBase)Settings.Default).Reset();
			}
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"User settings");
		}

		private void saveAndExitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			SaveDefaults();
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void Defaults_Resize(object sender, EventArgs e)
		{
			if (((Control)this).get_Width() > 50)
			{
				((Control)panel1).set_Width(((Control)this).get_Width() - 16);
			}
			if (((Control)this).get_Height() > 120)
			{
				((Control)panel1).set_Height(((Control)this).get_Height() - 60);
			}
		}

		private void linkC2A_LinkClicked(object sender, LinkLabelLinkClickedEventArgs e)
		{
			Process.Start(e.get_Link().get_LinkData().ToString());
		}

		private void SetC2ACatalog()
		{
			optSAO.set_Checked(((Control)optSAO).get_Text() == Settings.Default.C2ACatalog);
			optUCAC4.set_Checked(((Control)optUCAC4).get_Text() == Settings.Default.C2ACatalog);
			optPPMXL.set_Checked(((Control)optPPMXL).get_Text() == Settings.Default.C2ACatalog);
			optNOMAD.set_Checked(((Control)optNOMAD).get_Text() == Settings.Default.C2ACatalog);
		}

		private void optSAO_Click(object sender, EventArgs e)
		{
			Settings.Default.C2ACatalog = ((Control)optSAO).get_Text();
		}

		private void optUCAC4_Click(object sender, EventArgs e)
		{
			Settings.Default.C2ACatalog = ((Control)optUCAC4).get_Text();
			if (Settings.Default.UCAC4_Path.Length < 4)
			{
				StarCatalogueNotInOccult("UCAC4");
			}
		}

		private void optPPMXL_Click(object sender, EventArgs e)
		{
			Settings.Default.C2ACatalog = ((Control)optPPMXL).get_Text();
			if (Settings.Default.PPMXL_Path.Length < 4)
			{
				StarCatalogueNotInOccult("PPMXL");
			}
		}

		private void optNOMAD_Click(object sender, EventArgs e)
		{
			Settings.Default.C2ACatalog = ((Control)optNOMAD).get_Text();
			if (Settings.Default.NOMAD_Path.Length < 4)
			{
				StarCatalogueNotInOccult("NOMAD");
			}
		}

		private void StarCatalogueNotInOccult(string Catalog)
		{
			//IL_0065: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			if (Catalog == "NOMAD")
			{
				text = "the [full] version of ";
			}
			MessageBox.Show("You have selected the " + Catalog + " star catalogue to use in C2A.\r\n\r\nHowever you have not set Occult to use " + text + "that catalog, which suggests you may not have " + Catalog + " on your computer.\r\n\r\nThis is not a problem if you have set up C2A to use " + Catalog + ". Otherwise you should select a different catalog", "Catalogue missing", (MessageBoxButtons)0, (MessageBoxIcon)48);
		}

		private void updnMaximumSearchEvents_ValueChanged(object sender, EventArgs e)
		{
			MinorPlanetOccultationElements.MaximumNumEvents = (int)updnMaximumSearchEvents.get_Value();
		}

		private void cmdDownload_u4i_files_Click(object sender, EventArgs e)
		{
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			if (((Control)txtUCAC4).get_Text() == "")
			{
				MessageBox.Show("A directory for the UCAC4 catalogue has not been set. You cannot proceed until you have set a UCAC4 directory", "no UCAC4 directory", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			string text = ((Control)txtUCAC4).get_Text() + "\\u4i";
			if (!Directory.Exists(text))
			{
				Directory.CreateDirectory(text);
			}
			string occultServer = Settings.Default.OccultServer;
			string fileName = "u4_files_for_u4i.zip";
			string finalDestination = text;
			((Control)this).set_Cursor(Cursors.get_WaitCursor());
			http.DownloadHTTP(occultServer, fileName, finalDestination, unzip: true, gunzip: false, ShowMessages: false);
			((Control)this).set_Cursor(Cursors.get_Default());
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
			//IL_064c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0656: Expected O, but got Unknown
			//IL_0657: Unknown result type (might be due to invalid IL or missing references)
			//IL_0661: Expected O, but got Unknown
			//IL_0662: Unknown result type (might be due to invalid IL or missing references)
			//IL_066c: Expected O, but got Unknown
			//IL_066d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0677: Expected O, but got Unknown
			//IL_0678: Unknown result type (might be due to invalid IL or missing references)
			//IL_0682: Expected O, but got Unknown
			//IL_0683: Unknown result type (might be due to invalid IL or missing references)
			//IL_068d: Expected O, but got Unknown
			//IL_068e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0698: Expected O, but got Unknown
			//IL_0699: Unknown result type (might be due to invalid IL or missing references)
			//IL_06a3: Expected O, but got Unknown
			//IL_06a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_06ae: Expected O, but got Unknown
			//IL_06af: Unknown result type (might be due to invalid IL or missing references)
			//IL_06b9: Expected O, but got Unknown
			//IL_06ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_06c4: Expected O, but got Unknown
			//IL_06c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_06cf: Expected O, but got Unknown
			//IL_06d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_06da: Expected O, but got Unknown
			//IL_06db: Unknown result type (might be due to invalid IL or missing references)
			//IL_06e5: Expected O, but got Unknown
			//IL_06e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_06f0: Expected O, but got Unknown
			//IL_06f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_06fb: Expected O, but got Unknown
			//IL_06fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0706: Expected O, but got Unknown
			//IL_0707: Unknown result type (might be due to invalid IL or missing references)
			//IL_0711: Expected O, but got Unknown
			//IL_0712: Unknown result type (might be due to invalid IL or missing references)
			//IL_071c: Expected O, but got Unknown
			//IL_071d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0727: Expected O, but got Unknown
			//IL_0728: Unknown result type (might be due to invalid IL or missing references)
			//IL_0732: Expected O, but got Unknown
			//IL_0733: Unknown result type (might be due to invalid IL or missing references)
			//IL_073d: Expected O, but got Unknown
			//IL_073e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0748: Expected O, but got Unknown
			//IL_0749: Unknown result type (might be due to invalid IL or missing references)
			//IL_0753: Expected O, but got Unknown
			//IL_0754: Unknown result type (might be due to invalid IL or missing references)
			//IL_075e: Expected O, but got Unknown
			//IL_075f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0769: Expected O, but got Unknown
			//IL_076a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0774: Expected O, but got Unknown
			//IL_0775: Unknown result type (might be due to invalid IL or missing references)
			//IL_077f: Expected O, but got Unknown
			//IL_0780: Unknown result type (might be due to invalid IL or missing references)
			//IL_078a: Expected O, but got Unknown
			//IL_078b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0795: Expected O, but got Unknown
			//IL_0796: Unknown result type (might be due to invalid IL or missing references)
			//IL_07a0: Expected O, but got Unknown
			//IL_07a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ab: Expected O, but got Unknown
			//IL_07ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_07b6: Expected O, but got Unknown
			//IL_07b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c1: Expected O, but got Unknown
			//IL_07c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_07cc: Expected O, but got Unknown
			//IL_07cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_07d7: Expected O, but got Unknown
			//IL_07d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_07e2: Expected O, but got Unknown
			//IL_07e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_07ed: Expected O, but got Unknown
			//IL_07ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_07f8: Expected O, but got Unknown
			//IL_07f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0803: Expected O, but got Unknown
			//IL_0804: Unknown result type (might be due to invalid IL or missing references)
			//IL_080e: Expected O, but got Unknown
			//IL_080f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0819: Expected O, but got Unknown
			//IL_081a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0824: Expected O, but got Unknown
			//IL_0825: Unknown result type (might be due to invalid IL or missing references)
			//IL_082f: Expected O, but got Unknown
			//IL_0830: Unknown result type (might be due to invalid IL or missing references)
			//IL_083a: Expected O, but got Unknown
			//IL_083b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0845: Expected O, but got Unknown
			//IL_0846: Unknown result type (might be due to invalid IL or missing references)
			//IL_0850: Expected O, but got Unknown
			//IL_0851: Unknown result type (might be due to invalid IL or missing references)
			//IL_085b: Expected O, but got Unknown
			//IL_085c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0866: Expected O, but got Unknown
			//IL_0867: Unknown result type (might be due to invalid IL or missing references)
			//IL_0871: Expected O, but got Unknown
			//IL_0872: Unknown result type (might be due to invalid IL or missing references)
			//IL_087c: Expected O, but got Unknown
			//IL_087d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0887: Expected O, but got Unknown
			//IL_0888: Unknown result type (might be due to invalid IL or missing references)
			//IL_0892: Expected O, but got Unknown
			//IL_0893: Unknown result type (might be due to invalid IL or missing references)
			//IL_089d: Expected O, but got Unknown
			//IL_089e: Unknown result type (might be due to invalid IL or missing references)
			//IL_08a8: Expected O, but got Unknown
			//IL_08a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_08b3: Expected O, but got Unknown
			//IL_08b4: Unknown result type (might be due to invalid IL or missing references)
			//IL_08be: Expected O, but got Unknown
			//IL_08bf: Unknown result type (might be due to invalid IL or missing references)
			//IL_08c9: Expected O, but got Unknown
			//IL_08ca: Unknown result type (might be due to invalid IL or missing references)
			//IL_08d4: Expected O, but got Unknown
			//IL_08d5: Unknown result type (might be due to invalid IL or missing references)
			//IL_08df: Expected O, but got Unknown
			//IL_08e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_08ea: Expected O, but got Unknown
			//IL_08eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_08f5: Expected O, but got Unknown
			//IL_08f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0900: Expected O, but got Unknown
			//IL_0901: Unknown result type (might be due to invalid IL or missing references)
			//IL_090b: Expected O, but got Unknown
			//IL_090c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0916: Expected O, but got Unknown
			//IL_0917: Unknown result type (might be due to invalid IL or missing references)
			//IL_0921: Expected O, but got Unknown
			//IL_0922: Unknown result type (might be due to invalid IL or missing references)
			//IL_092c: Expected O, but got Unknown
			//IL_092d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0937: Expected O, but got Unknown
			//IL_0938: Unknown result type (might be due to invalid IL or missing references)
			//IL_0942: Expected O, but got Unknown
			//IL_0943: Unknown result type (might be due to invalid IL or missing references)
			//IL_094d: Expected O, but got Unknown
			//IL_094e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0958: Expected O, but got Unknown
			//IL_0959: Unknown result type (might be due to invalid IL or missing references)
			//IL_0963: Expected O, but got Unknown
			//IL_0964: Unknown result type (might be due to invalid IL or missing references)
			//IL_096e: Expected O, but got Unknown
			//IL_096f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0979: Expected O, but got Unknown
			//IL_097a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0984: Expected O, but got Unknown
			//IL_0985: Unknown result type (might be due to invalid IL or missing references)
			//IL_098f: Expected O, but got Unknown
			//IL_0990: Unknown result type (might be due to invalid IL or missing references)
			//IL_099a: Expected O, but got Unknown
			//IL_099b: Unknown result type (might be due to invalid IL or missing references)
			//IL_09a5: Expected O, but got Unknown
			//IL_09a6: Unknown result type (might be due to invalid IL or missing references)
			//IL_09b0: Expected O, but got Unknown
			//IL_09b1: Unknown result type (might be due to invalid IL or missing references)
			//IL_09bb: Expected O, but got Unknown
			//IL_09bc: Unknown result type (might be due to invalid IL or missing references)
			//IL_09c6: Expected O, but got Unknown
			//IL_09c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_09d1: Expected O, but got Unknown
			//IL_09d2: Unknown result type (might be due to invalid IL or missing references)
			//IL_09dc: Expected O, but got Unknown
			//IL_09dd: Unknown result type (might be due to invalid IL or missing references)
			//IL_09e7: Expected O, but got Unknown
			//IL_09e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_09f2: Expected O, but got Unknown
			//IL_09f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_09fd: Expected O, but got Unknown
			//IL_09fe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a08: Expected O, but got Unknown
			//IL_0a09: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a13: Expected O, but got Unknown
			//IL_0a14: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a1e: Expected O, but got Unknown
			//IL_0a1f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a29: Expected O, but got Unknown
			//IL_0a2a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a34: Expected O, but got Unknown
			//IL_0a35: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a3f: Expected O, but got Unknown
			//IL_0a40: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a4a: Expected O, but got Unknown
			//IL_0a4b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a55: Expected O, but got Unknown
			//IL_0a56: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a60: Expected O, but got Unknown
			//IL_0a61: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a6b: Expected O, but got Unknown
			//IL_0a6c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a76: Expected O, but got Unknown
			//IL_0a77: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a81: Expected O, but got Unknown
			//IL_0a82: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a8c: Expected O, but got Unknown
			//IL_0a8d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0a97: Expected O, but got Unknown
			//IL_0a98: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aa2: Expected O, but got Unknown
			//IL_0aa3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aad: Expected O, but got Unknown
			//IL_0aae: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ab8: Expected O, but got Unknown
			//IL_0ab9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ac3: Expected O, but got Unknown
			//IL_0ac4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ace: Expected O, but got Unknown
			//IL_0acf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ad9: Expected O, but got Unknown
			//IL_0ada: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ae4: Expected O, but got Unknown
			//IL_0ae5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0aef: Expected O, but got Unknown
			//IL_0af0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0afa: Expected O, but got Unknown
			//IL_0afb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b05: Expected O, but got Unknown
			//IL_0b06: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b10: Expected O, but got Unknown
			//IL_0b11: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b1b: Expected O, but got Unknown
			//IL_0b1c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b26: Expected O, but got Unknown
			//IL_0b27: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b31: Expected O, but got Unknown
			//IL_0b32: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b3c: Expected O, but got Unknown
			//IL_0b3d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b47: Expected O, but got Unknown
			//IL_0b48: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b52: Expected O, but got Unknown
			//IL_0b53: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b5d: Expected O, but got Unknown
			//IL_0b5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b68: Expected O, but got Unknown
			//IL_0b69: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b73: Expected O, but got Unknown
			//IL_0b74: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b7e: Expected O, but got Unknown
			//IL_0b7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b89: Expected O, but got Unknown
			//IL_0b8a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b94: Expected O, but got Unknown
			//IL_0b95: Unknown result type (might be due to invalid IL or missing references)
			//IL_0b9f: Expected O, but got Unknown
			//IL_0ba0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0baa: Expected O, but got Unknown
			//IL_0bab: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bb5: Expected O, but got Unknown
			//IL_0bb6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bc0: Expected O, but got Unknown
			//IL_0bc1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bcb: Expected O, but got Unknown
			//IL_0bcc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bd6: Expected O, but got Unknown
			//IL_0bd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0be1: Expected O, but got Unknown
			//IL_0be2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bec: Expected O, but got Unknown
			//IL_0bed: Unknown result type (might be due to invalid IL or missing references)
			//IL_0bf7: Expected O, but got Unknown
			//IL_0bf8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c02: Expected O, but got Unknown
			//IL_0c03: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c0d: Expected O, but got Unknown
			//IL_0c0e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c18: Expected O, but got Unknown
			//IL_0c19: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c23: Expected O, but got Unknown
			//IL_0c24: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c2e: Expected O, but got Unknown
			//IL_0c2f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c39: Expected O, but got Unknown
			//IL_0c3a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c44: Expected O, but got Unknown
			//IL_0c45: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c4f: Expected O, but got Unknown
			//IL_0c50: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c5a: Expected O, but got Unknown
			//IL_0c5b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c65: Expected O, but got Unknown
			//IL_0c66: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c70: Expected O, but got Unknown
			//IL_0c71: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c7b: Expected O, but got Unknown
			//IL_0c7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c86: Expected O, but got Unknown
			//IL_0c87: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c91: Expected O, but got Unknown
			//IL_0c92: Unknown result type (might be due to invalid IL or missing references)
			//IL_0c9c: Expected O, but got Unknown
			//IL_0c9d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ca7: Expected O, but got Unknown
			//IL_0ca8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cb2: Expected O, but got Unknown
			//IL_0cb3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cbd: Expected O, but got Unknown
			//IL_0cbe: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cc8: Expected O, but got Unknown
			//IL_0cc9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cd3: Expected O, but got Unknown
			//IL_0cd4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cde: Expected O, but got Unknown
			//IL_0cdf: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ce9: Expected O, but got Unknown
			//IL_0cea: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cf4: Expected O, but got Unknown
			//IL_0cf5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0cff: Expected O, but got Unknown
			//IL_0d00: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d0a: Expected O, but got Unknown
			//IL_0d0b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d15: Expected O, but got Unknown
			//IL_0d16: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d20: Expected O, but got Unknown
			//IL_0d21: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d2b: Expected O, but got Unknown
			//IL_0d2c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d36: Expected O, but got Unknown
			//IL_0d37: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d41: Expected O, but got Unknown
			//IL_0d42: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d4c: Expected O, but got Unknown
			//IL_0d4d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d57: Expected O, but got Unknown
			//IL_0d58: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d62: Expected O, but got Unknown
			//IL_0d63: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d6d: Expected O, but got Unknown
			//IL_0d6e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d78: Expected O, but got Unknown
			//IL_0d79: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d83: Expected O, but got Unknown
			//IL_0d84: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d8e: Expected O, but got Unknown
			//IL_0d8f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0d99: Expected O, but got Unknown
			//IL_0d9a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0da4: Expected O, but got Unknown
			//IL_0da5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0daf: Expected O, but got Unknown
			//IL_0db0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dba: Expected O, but got Unknown
			//IL_0dbb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dc5: Expected O, but got Unknown
			//IL_0dc6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dd0: Expected O, but got Unknown
			//IL_0dd1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ddb: Expected O, but got Unknown
			//IL_0ddc: Unknown result type (might be due to invalid IL or missing references)
			//IL_0de6: Expected O, but got Unknown
			//IL_0de7: Unknown result type (might be due to invalid IL or missing references)
			//IL_0df1: Expected O, but got Unknown
			//IL_0df2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0dfc: Expected O, but got Unknown
			//IL_0dfd: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e07: Expected O, but got Unknown
			//IL_0e08: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e12: Expected O, but got Unknown
			//IL_0e13: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e1d: Expected O, but got Unknown
			//IL_0e1e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e28: Expected O, but got Unknown
			//IL_0e29: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e33: Expected O, but got Unknown
			//IL_0e34: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e3e: Expected O, but got Unknown
			//IL_0e3f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e49: Expected O, but got Unknown
			//IL_0e4a: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e54: Expected O, but got Unknown
			//IL_0e55: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e5f: Expected O, but got Unknown
			//IL_0e60: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e6a: Expected O, but got Unknown
			//IL_0e6b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e75: Expected O, but got Unknown
			//IL_0e76: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e80: Expected O, but got Unknown
			//IL_0e81: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e8b: Expected O, but got Unknown
			//IL_0e8c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0e96: Expected O, but got Unknown
			//IL_0e97: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ea1: Expected O, but got Unknown
			//IL_0ea2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eac: Expected O, but got Unknown
			//IL_0ead: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eb7: Expected O, but got Unknown
			//IL_0eb8: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ec2: Expected O, but got Unknown
			//IL_0ec3: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ecd: Expected O, but got Unknown
			//IL_0ece: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ed8: Expected O, but got Unknown
			//IL_0ed9: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ee3: Expected O, but got Unknown
			//IL_0ee4: Unknown result type (might be due to invalid IL or missing references)
			//IL_0eee: Expected O, but got Unknown
			//IL_0eef: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ef9: Expected O, but got Unknown
			//IL_0efa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f04: Expected O, but got Unknown
			//IL_0f05: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f0f: Expected O, but got Unknown
			//IL_0f10: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f1a: Expected O, but got Unknown
			//IL_0f1b: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f25: Expected O, but got Unknown
			//IL_0f26: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f30: Expected O, but got Unknown
			//IL_0f31: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f3b: Expected O, but got Unknown
			//IL_0f3c: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f46: Expected O, but got Unknown
			//IL_0f47: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f51: Expected O, but got Unknown
			//IL_0f52: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f5c: Expected O, but got Unknown
			//IL_0f5d: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f67: Expected O, but got Unknown
			//IL_0f68: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f72: Expected O, but got Unknown
			//IL_0f73: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f7d: Expected O, but got Unknown
			//IL_0f7e: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f88: Expected O, but got Unknown
			//IL_0f89: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f93: Expected O, but got Unknown
			//IL_0f94: Unknown result type (might be due to invalid IL or missing references)
			//IL_0f9e: Expected O, but got Unknown
			//IL_0f9f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fa9: Expected O, but got Unknown
			//IL_0faa: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fb4: Expected O, but got Unknown
			//IL_0fb5: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fbf: Expected O, but got Unknown
			//IL_0fc0: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fca: Expected O, but got Unknown
			//IL_0fcb: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fd5: Expected O, but got Unknown
			//IL_0fd6: Unknown result type (might be due to invalid IL or missing references)
			//IL_0fe0: Expected O, but got Unknown
			//IL_0fe1: Unknown result type (might be due to invalid IL or missing references)
			//IL_0feb: Expected O, but got Unknown
			//IL_0fec: Unknown result type (might be due to invalid IL or missing references)
			//IL_0ff6: Expected O, but got Unknown
			//IL_0ff7: Unknown result type (might be due to invalid IL or missing references)
			//IL_1001: Expected O, but got Unknown
			//IL_1002: Unknown result type (might be due to invalid IL or missing references)
			//IL_100c: Expected O, but got Unknown
			//IL_100d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1017: Expected O, but got Unknown
			//IL_1018: Unknown result type (might be due to invalid IL or missing references)
			//IL_1022: Expected O, but got Unknown
			//IL_1023: Unknown result type (might be due to invalid IL or missing references)
			//IL_102d: Expected O, but got Unknown
			//IL_102e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1038: Expected O, but got Unknown
			//IL_1039: Unknown result type (might be due to invalid IL or missing references)
			//IL_1043: Expected O, but got Unknown
			//IL_1044: Unknown result type (might be due to invalid IL or missing references)
			//IL_104e: Expected O, but got Unknown
			//IL_104f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1059: Expected O, but got Unknown
			//IL_105a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1064: Expected O, but got Unknown
			//IL_1065: Unknown result type (might be due to invalid IL or missing references)
			//IL_106f: Expected O, but got Unknown
			//IL_1070: Unknown result type (might be due to invalid IL or missing references)
			//IL_107a: Expected O, but got Unknown
			//IL_107b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1085: Expected O, but got Unknown
			//IL_1086: Unknown result type (might be due to invalid IL or missing references)
			//IL_1090: Expected O, but got Unknown
			//IL_1091: Unknown result type (might be due to invalid IL or missing references)
			//IL_109b: Expected O, but got Unknown
			//IL_109c: Unknown result type (might be due to invalid IL or missing references)
			//IL_10a6: Expected O, but got Unknown
			//IL_10a7: Unknown result type (might be due to invalid IL or missing references)
			//IL_10b1: Expected O, but got Unknown
			//IL_10b2: Unknown result type (might be due to invalid IL or missing references)
			//IL_10bc: Expected O, but got Unknown
			//IL_10bd: Unknown result type (might be due to invalid IL or missing references)
			//IL_10c7: Expected O, but got Unknown
			//IL_10c8: Unknown result type (might be due to invalid IL or missing references)
			//IL_10d2: Expected O, but got Unknown
			//IL_10d3: Unknown result type (might be due to invalid IL or missing references)
			//IL_10dd: Expected O, but got Unknown
			//IL_10de: Unknown result type (might be due to invalid IL or missing references)
			//IL_10e8: Expected O, but got Unknown
			//IL_10e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_10f3: Expected O, but got Unknown
			//IL_10f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_10fe: Expected O, but got Unknown
			//IL_10ff: Unknown result type (might be due to invalid IL or missing references)
			//IL_1109: Expected O, but got Unknown
			//IL_110a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1114: Expected O, but got Unknown
			//IL_1115: Unknown result type (might be due to invalid IL or missing references)
			//IL_111f: Expected O, but got Unknown
			//IL_1120: Unknown result type (might be due to invalid IL or missing references)
			//IL_112a: Expected O, but got Unknown
			//IL_112b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1135: Expected O, but got Unknown
			//IL_1136: Unknown result type (might be due to invalid IL or missing references)
			//IL_1140: Expected O, but got Unknown
			//IL_1141: Unknown result type (might be due to invalid IL or missing references)
			//IL_114b: Expected O, but got Unknown
			//IL_114c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1156: Expected O, but got Unknown
			//IL_1157: Unknown result type (might be due to invalid IL or missing references)
			//IL_1161: Expected O, but got Unknown
			//IL_1162: Unknown result type (might be due to invalid IL or missing references)
			//IL_116c: Expected O, but got Unknown
			//IL_116d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1177: Expected O, but got Unknown
			//IL_1178: Unknown result type (might be due to invalid IL or missing references)
			//IL_1182: Expected O, but got Unknown
			//IL_1183: Unknown result type (might be due to invalid IL or missing references)
			//IL_118d: Expected O, but got Unknown
			//IL_118e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1198: Expected O, but got Unknown
			//IL_1199: Unknown result type (might be due to invalid IL or missing references)
			//IL_11a3: Expected O, but got Unknown
			//IL_11a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_11ae: Expected O, but got Unknown
			//IL_11af: Unknown result type (might be due to invalid IL or missing references)
			//IL_11b9: Expected O, but got Unknown
			//IL_11ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_11c4: Expected O, but got Unknown
			//IL_11c5: Unknown result type (might be due to invalid IL or missing references)
			//IL_11cf: Expected O, but got Unknown
			//IL_11d0: Unknown result type (might be due to invalid IL or missing references)
			//IL_11da: Expected O, but got Unknown
			//IL_11db: Unknown result type (might be due to invalid IL or missing references)
			//IL_11e5: Expected O, but got Unknown
			//IL_11e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_11f0: Expected O, but got Unknown
			//IL_11f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_11fb: Expected O, but got Unknown
			//IL_11fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_1206: Expected O, but got Unknown
			//IL_1207: Unknown result type (might be due to invalid IL or missing references)
			//IL_1211: Expected O, but got Unknown
			//IL_1212: Unknown result type (might be due to invalid IL or missing references)
			//IL_121c: Expected O, but got Unknown
			//IL_121d: Unknown result type (might be due to invalid IL or missing references)
			//IL_1227: Expected O, but got Unknown
			//IL_1228: Unknown result type (might be due to invalid IL or missing references)
			//IL_1232: Expected O, but got Unknown
			//IL_1233: Unknown result type (might be due to invalid IL or missing references)
			//IL_123d: Expected O, but got Unknown
			//IL_123e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1248: Expected O, but got Unknown
			//IL_1249: Unknown result type (might be due to invalid IL or missing references)
			//IL_1253: Expected O, but got Unknown
			//IL_1254: Unknown result type (might be due to invalid IL or missing references)
			//IL_125e: Expected O, but got Unknown
			//IL_125f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1269: Expected O, but got Unknown
			//IL_126a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1274: Expected O, but got Unknown
			//IL_1275: Unknown result type (might be due to invalid IL or missing references)
			//IL_127f: Expected O, but got Unknown
			//IL_1280: Unknown result type (might be due to invalid IL or missing references)
			//IL_128a: Expected O, but got Unknown
			//IL_128b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1295: Expected O, but got Unknown
			//IL_1296: Unknown result type (might be due to invalid IL or missing references)
			//IL_12a0: Expected O, but got Unknown
			//IL_12a1: Unknown result type (might be due to invalid IL or missing references)
			//IL_12ab: Expected O, but got Unknown
			//IL_12ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_12b6: Expected O, but got Unknown
			//IL_12b7: Unknown result type (might be due to invalid IL or missing references)
			//IL_12c1: Expected O, but got Unknown
			//IL_12c2: Unknown result type (might be due to invalid IL or missing references)
			//IL_12cc: Expected O, but got Unknown
			//IL_12cd: Unknown result type (might be due to invalid IL or missing references)
			//IL_12d7: Expected O, but got Unknown
			//IL_12d8: Unknown result type (might be due to invalid IL or missing references)
			//IL_12e2: Expected O, but got Unknown
			//IL_12e3: Unknown result type (might be due to invalid IL or missing references)
			//IL_12ed: Expected O, but got Unknown
			//IL_12ee: Unknown result type (might be due to invalid IL or missing references)
			//IL_12f8: Expected O, but got Unknown
			//IL_12f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_1303: Expected O, but got Unknown
			//IL_1304: Unknown result type (might be due to invalid IL or missing references)
			//IL_130e: Expected O, but got Unknown
			//IL_130f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1319: Expected O, but got Unknown
			//IL_131a: Unknown result type (might be due to invalid IL or missing references)
			//IL_1324: Expected O, but got Unknown
			//IL_1325: Unknown result type (might be due to invalid IL or missing references)
			//IL_132f: Expected O, but got Unknown
			//IL_1330: Unknown result type (might be due to invalid IL or missing references)
			//IL_133a: Expected O, but got Unknown
			//IL_133b: Unknown result type (might be due to invalid IL or missing references)
			//IL_1345: Expected O, but got Unknown
			//IL_1346: Unknown result type (might be due to invalid IL or missing references)
			//IL_1350: Expected O, but got Unknown
			//IL_1351: Unknown result type (might be due to invalid IL or missing references)
			//IL_135b: Expected O, but got Unknown
			//IL_135c: Unknown result type (might be due to invalid IL or missing references)
			//IL_1366: Expected O, but got Unknown
			//IL_1367: Unknown result type (might be due to invalid IL or missing references)
			//IL_1371: Expected O, but got Unknown
			//IL_1378: Unknown result type (might be due to invalid IL or missing references)
			//IL_1382: Expected O, but got Unknown
			//IL_1383: Unknown result type (might be due to invalid IL or missing references)
			//IL_138d: Expected O, but got Unknown
			//IL_138e: Unknown result type (might be due to invalid IL or missing references)
			//IL_1398: Expected O, but got Unknown
			//IL_1399: Unknown result type (might be due to invalid IL or missing references)
			//IL_13a3: Expected O, but got Unknown
			//IL_13a4: Unknown result type (might be due to invalid IL or missing references)
			//IL_13ae: Expected O, but got Unknown
			//IL_13af: Unknown result type (might be due to invalid IL or missing references)
			//IL_13b9: Expected O, but got Unknown
			//IL_13ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_13c4: Expected O, but got Unknown
			//IL_1af3: Unknown result type (might be due to invalid IL or missing references)
			//IL_1afd: Expected O, but got Unknown
			//IL_1d94: Unknown result type (might be due to invalid IL or missing references)
			//IL_1d9e: Expected O, but got Unknown
			//IL_1f1f: Unknown result type (might be due to invalid IL or missing references)
			//IL_1f29: Expected O, but got Unknown
			//IL_1fbf: Unknown result type (might be due to invalid IL or missing references)
			//IL_1fc9: Expected O, but got Unknown
			//IL_204a: Unknown result type (might be due to invalid IL or missing references)
			//IL_2054: Expected O, but got Unknown
			//IL_21e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_21f2: Expected O, but got Unknown
			//IL_2bb5: Unknown result type (might be due to invalid IL or missing references)
			//IL_2bbf: Expected O, but got Unknown
			//IL_2cdc: Unknown result type (might be due to invalid IL or missing references)
			//IL_2ce6: Expected O, but got Unknown
			//IL_2e00: Unknown result type (might be due to invalid IL or missing references)
			//IL_2e0a: Expected O, but got Unknown
			//IL_2f27: Unknown result type (might be due to invalid IL or missing references)
			//IL_2f31: Expected O, but got Unknown
			//IL_2fca: Unknown result type (might be due to invalid IL or missing references)
			//IL_2fd4: Expected O, but got Unknown
			//IL_30f0: Unknown result type (might be due to invalid IL or missing references)
			//IL_30fa: Expected O, but got Unknown
			//IL_32ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_32b6: Expected O, but got Unknown
			//IL_345a: Unknown result type (might be due to invalid IL or missing references)
			//IL_3464: Expected O, but got Unknown
			//IL_34fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_3507: Expected O, but got Unknown
			//IL_3624: Unknown result type (might be due to invalid IL or missing references)
			//IL_362e: Expected O, but got Unknown
			//IL_3762: Unknown result type (might be due to invalid IL or missing references)
			//IL_376c: Expected O, but got Unknown
			//IL_39b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_39bf: Expected O, but got Unknown
			//IL_3a6f: Unknown result type (might be due to invalid IL or missing references)
			//IL_3a79: Expected O, but got Unknown
			//IL_3b29: Unknown result type (might be due to invalid IL or missing references)
			//IL_3b33: Expected O, but got Unknown
			//IL_3be6: Unknown result type (might be due to invalid IL or missing references)
			//IL_3bf0: Expected O, but got Unknown
			//IL_3dc7: Unknown result type (might be due to invalid IL or missing references)
			//IL_3dd1: Expected O, but got Unknown
			//IL_3e81: Unknown result type (might be due to invalid IL or missing references)
			//IL_3e8b: Expected O, but got Unknown
			//IL_404d: Unknown result type (might be due to invalid IL or missing references)
			//IL_4057: Expected O, but got Unknown
			//IL_4117: Unknown result type (might be due to invalid IL or missing references)
			//IL_4121: Expected O, but got Unknown
			//IL_43f1: Unknown result type (might be due to invalid IL or missing references)
			//IL_43fb: Expected O, but got Unknown
			//IL_44bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_44c5: Expected O, but got Unknown
			//IL_4585: Unknown result type (might be due to invalid IL or missing references)
			//IL_458f: Expected O, but got Unknown
			//IL_4652: Unknown result type (might be due to invalid IL or missing references)
			//IL_465c: Expected O, but got Unknown
			//IL_49ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_49c4: Expected O, but got Unknown
			//IL_4a64: Unknown result type (might be due to invalid IL or missing references)
			//IL_4a6e: Expected O, but got Unknown
			//IL_4f7c: Unknown result type (might be due to invalid IL or missing references)
			//IL_4f86: Expected O, but got Unknown
			//IL_56c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_56cb: Expected O, but got Unknown
			//IL_5b1a: Unknown result type (might be due to invalid IL or missing references)
			//IL_5b24: Expected O, but got Unknown
			//IL_5bdb: Unknown result type (might be due to invalid IL or missing references)
			//IL_5be5: Expected O, but got Unknown
			//IL_5eb5: Unknown result type (might be due to invalid IL or missing references)
			//IL_5ebf: Expected O, but got Unknown
			//IL_5f61: Unknown result type (might be due to invalid IL or missing references)
			//IL_5f6b: Expected O, but got Unknown
			//IL_6090: Unknown result type (might be due to invalid IL or missing references)
			//IL_609a: Expected O, but got Unknown
			//IL_62d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_62e0: Expected O, but got Unknown
			//IL_6aea: Unknown result type (might be due to invalid IL or missing references)
			//IL_6af4: Expected O, but got Unknown
			//IL_6bd7: Unknown result type (might be due to invalid IL or missing references)
			//IL_6be1: Expected O, but got Unknown
			//IL_6d30: Unknown result type (might be due to invalid IL or missing references)
			//IL_6d3a: Expected O, but got Unknown
			//IL_6e08: Unknown result type (might be due to invalid IL or missing references)
			//IL_6e12: Expected O, but got Unknown
			//IL_6edd: Unknown result type (might be due to invalid IL or missing references)
			//IL_6ee7: Expected O, but got Unknown
			//IL_6fd6: Unknown result type (might be due to invalid IL or missing references)
			//IL_6fe0: Expected O, but got Unknown
			//IL_71ad: Unknown result type (might be due to invalid IL or missing references)
			//IL_71b7: Expected O, but got Unknown
			//IL_7282: Unknown result type (might be due to invalid IL or missing references)
			//IL_728c: Expected O, but got Unknown
			//IL_7357: Unknown result type (might be due to invalid IL or missing references)
			//IL_7361: Expected O, but got Unknown
			//IL_742c: Unknown result type (might be due to invalid IL or missing references)
			//IL_7436: Expected O, but got Unknown
			//IL_7501: Unknown result type (might be due to invalid IL or missing references)
			//IL_750b: Expected O, but got Unknown
			//IL_7b7a: Unknown result type (might be due to invalid IL or missing references)
			//IL_7b84: Expected O, but got Unknown
			//IL_88c7: Unknown result type (might be due to invalid IL or missing references)
			//IL_88d1: Expected O, but got Unknown
			//IL_8cb0: Unknown result type (might be due to invalid IL or missing references)
			//IL_8cba: Expected O, but got Unknown
			//IL_8f51: Unknown result type (might be due to invalid IL or missing references)
			//IL_8f5b: Expected O, but got Unknown
			//IL_90f6: Unknown result type (might be due to invalid IL or missing references)
			//IL_9100: Expected O, but got Unknown
			//IL_939c: Unknown result type (might be due to invalid IL or missing references)
			//IL_93a6: Expected O, but got Unknown
			//IL_9ead: Unknown result type (might be due to invalid IL or missing references)
			//IL_9eb7: Expected O, but got Unknown
			//IL_9fdd: Unknown result type (might be due to invalid IL or missing references)
			//IL_9fe7: Expected O, but got Unknown
			//IL_a0c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_a0d3: Expected O, but got Unknown
			//IL_a1b5: Unknown result type (might be due to invalid IL or missing references)
			//IL_a1bf: Expected O, but got Unknown
			//IL_a29e: Unknown result type (might be due to invalid IL or missing references)
			//IL_a2a8: Expected O, but got Unknown
			//IL_a50b: Unknown result type (might be due to invalid IL or missing references)
			//IL_a515: Expected O, but got Unknown
			//IL_a679: Unknown result type (might be due to invalid IL or missing references)
			//IL_a683: Expected O, but got Unknown
			//IL_a7ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_a7f4: Expected O, but got Unknown
			//IL_a955: Unknown result type (might be due to invalid IL or missing references)
			//IL_a95f: Expected O, but got Unknown
			//IL_aac0: Unknown result type (might be due to invalid IL or missing references)
			//IL_aaca: Expected O, but got Unknown
			//IL_ac2e: Unknown result type (might be due to invalid IL or missing references)
			//IL_ac38: Expected O, but got Unknown
			//IL_ad1b: Unknown result type (might be due to invalid IL or missing references)
			//IL_ad25: Expected O, but got Unknown
			//IL_ae05: Unknown result type (might be due to invalid IL or missing references)
			//IL_ae0f: Expected O, but got Unknown
			//IL_aeef: Unknown result type (might be due to invalid IL or missing references)
			//IL_aef9: Expected O, but got Unknown
			//IL_afd9: Unknown result type (might be due to invalid IL or missing references)
			//IL_afe3: Expected O, but got Unknown
			//IL_b0c3: Unknown result type (might be due to invalid IL or missing references)
			//IL_b0cd: Expected O, but got Unknown
			//IL_b1b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_b1ba: Expected O, but got Unknown
			//IL_b2a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_b2aa: Expected O, but got Unknown
			//IL_b38a: Unknown result type (might be due to invalid IL or missing references)
			//IL_b394: Expected O, but got Unknown
			//IL_b47a: Unknown result type (might be due to invalid IL or missing references)
			//IL_b484: Expected O, but got Unknown
			//IL_b564: Unknown result type (might be due to invalid IL or missing references)
			//IL_b56e: Expected O, but got Unknown
			//IL_b651: Unknown result type (might be due to invalid IL or missing references)
			//IL_b65b: Expected O, but got Unknown
			//IL_b73b: Unknown result type (might be due to invalid IL or missing references)
			//IL_b745: Expected O, but got Unknown
			//IL_b828: Unknown result type (might be due to invalid IL or missing references)
			//IL_b832: Expected O, but got Unknown
			//IL_b915: Unknown result type (might be due to invalid IL or missing references)
			//IL_b91f: Expected O, but got Unknown
			//IL_ba05: Unknown result type (might be due to invalid IL or missing references)
			//IL_ba0f: Expected O, but got Unknown
			//IL_baf5: Unknown result type (might be due to invalid IL or missing references)
			//IL_baff: Expected O, but got Unknown
			//IL_bbe8: Unknown result type (might be due to invalid IL or missing references)
			//IL_bbf2: Expected O, but got Unknown
			//IL_bcd8: Unknown result type (might be due to invalid IL or missing references)
			//IL_bce2: Expected O, but got Unknown
			//IL_bdc8: Unknown result type (might be due to invalid IL or missing references)
			//IL_bdd2: Expected O, but got Unknown
			//IL_beb8: Unknown result type (might be due to invalid IL or missing references)
			//IL_bec2: Expected O, but got Unknown
			//IL_bfa2: Unknown result type (might be due to invalid IL or missing references)
			//IL_bfac: Expected O, but got Unknown
			//IL_c092: Unknown result type (might be due to invalid IL or missing references)
			//IL_c09c: Expected O, but got Unknown
			//IL_c271: Unknown result type (might be due to invalid IL or missing references)
			//IL_c27b: Expected O, but got Unknown
			//IL_c3c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_c3ce: Expected O, but got Unknown
			//IL_c629: Unknown result type (might be due to invalid IL or missing references)
			//IL_c633: Expected O, but got Unknown
			//IL_c6f3: Unknown result type (might be due to invalid IL or missing references)
			//IL_c6fd: Expected O, but got Unknown
			//IL_c7ba: Unknown result type (might be due to invalid IL or missing references)
			//IL_c7c4: Expected O, but got Unknown
			//IL_cd83: Unknown result type (might be due to invalid IL or missing references)
			//IL_cd8d: Expected O, but got Unknown
			//IL_cf7b: Unknown result type (might be due to invalid IL or missing references)
			//IL_cf85: Expected O, but got Unknown
			//IL_d0f9: Unknown result type (might be due to invalid IL or missing references)
			//IL_d103: Expected O, but got Unknown
			//IL_d33f: Unknown result type (might be due to invalid IL or missing references)
			//IL_d349: Expected O, but got Unknown
			//IL_d4c4: Unknown result type (might be due to invalid IL or missing references)
			//IL_d4ce: Expected O, but got Unknown
			//IL_d689: Unknown result type (might be due to invalid IL or missing references)
			//IL_d693: Expected O, but got Unknown
			//IL_e48d: Unknown result type (might be due to invalid IL or missing references)
			//IL_e497: Expected O, but got Unknown
			//IL_e5a0: Unknown result type (might be due to invalid IL or missing references)
			//IL_e5aa: Expected O, but got Unknown
			//IL_e656: Unknown result type (might be due to invalid IL or missing references)
			//IL_e660: Expected O, but got Unknown
			//IL_e80f: Unknown result type (might be due to invalid IL or missing references)
			//IL_e819: Expected O, but got Unknown
			//IL_e8e9: Unknown result type (might be due to invalid IL or missing references)
			//IL_e8f3: Expected O, but got Unknown
			//IL_eba0: Unknown result type (might be due to invalid IL or missing references)
			//IL_ebaa: Expected O, but got Unknown
			//IL_ec6b: Unknown result type (might be due to invalid IL or missing references)
			//IL_ec75: Expected O, but got Unknown
			//IL_ee4c: Unknown result type (might be due to invalid IL or missing references)
			//IL_ee56: Expected O, but got Unknown
			//IL_efe8: Unknown result type (might be due to invalid IL or missing references)
			//IL_eff2: Expected O, but got Unknown
			//IL_f0a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_f0b3: Expected O, but got Unknown
			//IL_f1c1: Unknown result type (might be due to invalid IL or missing references)
			//IL_f1cb: Expected O, but got Unknown
			//IL_f332: Unknown result type (might be due to invalid IL or missing references)
			//IL_f33c: Expected O, but got Unknown
			//IL_f3dc: Unknown result type (might be due to invalid IL or missing references)
			//IL_f3e6: Expected O, but got Unknown
			//IL_f4f4: Unknown result type (might be due to invalid IL or missing references)
			//IL_f4fe: Expected O, but got Unknown
			//IL_f64b: Unknown result type (might be due to invalid IL or missing references)
			//IL_f655: Expected O, but got Unknown
			//IL_f739: Unknown result type (might be due to invalid IL or missing references)
			//IL_f743: Expected O, but got Unknown
			//IL_f894: Unknown result type (might be due to invalid IL or missing references)
			//IL_f89e: Expected O, but got Unknown
			//IL_f93e: Unknown result type (might be due to invalid IL or missing references)
			//IL_f948: Expected O, but got Unknown
			//IL_fa4a: Unknown result type (might be due to invalid IL or missing references)
			//IL_fa54: Expected O, but got Unknown
			//IL_fb84: Unknown result type (might be due to invalid IL or missing references)
			//IL_fb8e: Expected O, but got Unknown
			//IL_fc6f: Unknown result type (might be due to invalid IL or missing references)
			//IL_fc79: Expected O, but got Unknown
			//IL_fdde: Unknown result type (might be due to invalid IL or missing references)
			//IL_fde8: Expected O, but got Unknown
			//IL_ff6b: Unknown result type (might be due to invalid IL or missing references)
			//IL_ff75: Expected O, but got Unknown
			//IL_10056: Unknown result type (might be due to invalid IL or missing references)
			//IL_10060: Expected O, but got Unknown
			//IL_103fc: Unknown result type (might be due to invalid IL or missing references)
			//IL_10406: Expected O, but got Unknown
			//IL_10616: Unknown result type (might be due to invalid IL or missing references)
			//IL_10620: Expected O, but got Unknown
			//IL_107ac: Unknown result type (might be due to invalid IL or missing references)
			//IL_107b6: Expected O, but got Unknown
			//IL_109c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_109d0: Expected O, but got Unknown
			//IL_10be0: Unknown result type (might be due to invalid IL or missing references)
			//IL_10bea: Expected O, but got Unknown
			//IL_10d76: Unknown result type (might be due to invalid IL or missing references)
			//IL_10d80: Expected O, but got Unknown
			//IL_10e88: Unknown result type (might be due to invalid IL or missing references)
			//IL_10e92: Expected O, but got Unknown
			//IL_110e0: Unknown result type (might be due to invalid IL or missing references)
			//IL_110ea: Expected O, but got Unknown
			//IL_11202: Unknown result type (might be due to invalid IL or missing references)
			//IL_1120c: Expected O, but got Unknown
			//IL_1130c: Unknown result type (might be due to invalid IL or missing references)
			//IL_11316: Expected O, but got Unknown
			//IL_115c6: Unknown result type (might be due to invalid IL or missing references)
			//IL_115d0: Expected O, but got Unknown
			//IL_11ec3: Unknown result type (might be due to invalid IL or missing references)
			//IL_11ecd: Expected O, but got Unknown
			//IL_12215: Unknown result type (might be due to invalid IL or missing references)
			//IL_1221f: Expected O, but got Unknown
			//IL_12338: Unknown result type (might be due to invalid IL or missing references)
			//IL_12342: Expected O, but got Unknown
			//IL_12d7f: Unknown result type (might be due to invalid IL or missing references)
			//IL_12d89: Expected O, but got Unknown
			//IL_12e68: Unknown result type (might be due to invalid IL or missing references)
			//IL_12e72: Expected O, but got Unknown
			//IL_12f57: Unknown result type (might be due to invalid IL or missing references)
			//IL_12f61: Expected O, but got Unknown
			//IL_130b0: Unknown result type (might be due to invalid IL or missing references)
			//IL_130ba: Expected O, but got Unknown
			//IL_138e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_138f0: Expected O, but got Unknown
			//IL_13a61: Unknown result type (might be due to invalid IL or missing references)
			//IL_13a6b: Expected O, but got Unknown
			//IL_13c5d: Unknown result type (might be due to invalid IL or missing references)
			//IL_13c67: Expected O, but got Unknown
			//IL_13dd0: Unknown result type (might be due to invalid IL or missing references)
			//IL_13dda: Expected O, but got Unknown
			//IL_13f47: Unknown result type (might be due to invalid IL or missing references)
			//IL_13f51: Expected O, but got Unknown
			//IL_14085: Unknown result type (might be due to invalid IL or missing references)
			//IL_1408f: Expected O, but got Unknown
			//IL_14172: Unknown result type (might be due to invalid IL or missing references)
			//IL_1417c: Expected O, but got Unknown
			//IL_14274: Unknown result type (might be due to invalid IL or missing references)
			//IL_1427e: Expected O, but got Unknown
			//IL_14361: Unknown result type (might be due to invalid IL or missing references)
			//IL_1436b: Expected O, but got Unknown
			//IL_1446f: Unknown result type (might be due to invalid IL or missing references)
			//IL_14479: Expected O, but got Unknown
			//IL_145e6: Unknown result type (might be due to invalid IL or missing references)
			//IL_145f0: Expected O, but got Unknown
			//IL_1475c: Unknown result type (might be due to invalid IL or missing references)
			//IL_14766: Expected O, but got Unknown
			//IL_14816: Unknown result type (might be due to invalid IL or missing references)
			//IL_14820: Expected O, but got Unknown
			//IL_148fd: Unknown result type (might be due to invalid IL or missing references)
			//IL_14907: Expected O, but got Unknown
			//IL_149ea: Unknown result type (might be due to invalid IL or missing references)
			//IL_149f4: Expected O, but got Unknown
			//IL_14ad0: Unknown result type (might be due to invalid IL or missing references)
			//IL_14ada: Expected O, but got Unknown
			//IL_14d16: Unknown result type (might be due to invalid IL or missing references)
			//IL_14d20: Expected O, but got Unknown
			//IL_14e21: Unknown result type (might be due to invalid IL or missing references)
			//IL_14e2b: Expected O, but got Unknown
			//IL_14ed6: Unknown result type (might be due to invalid IL or missing references)
			//IL_14ee0: Expected O, but got Unknown
			//IL_14f5e: Unknown result type (might be due to invalid IL or missing references)
			//IL_14f68: Expected O, but got Unknown
			//IL_1508b: Unknown result type (might be due to invalid IL or missing references)
			//IL_15095: Expected O, but got Unknown
			//IL_15135: Unknown result type (might be due to invalid IL or missing references)
			//IL_1513f: Expected O, but got Unknown
			//IL_152ab: Unknown result type (might be due to invalid IL or missing references)
			//IL_152b5: Expected O, but got Unknown
			//IL_15365: Unknown result type (might be due to invalid IL or missing references)
			//IL_1536f: Expected O, but got Unknown
			//IL_15467: Unknown result type (might be due to invalid IL or missing references)
			//IL_15471: Expected O, but got Unknown
			//IL_155d1: Unknown result type (might be due to invalid IL or missing references)
			//IL_155db: Expected O, but got Unknown
			//IL_156a9: Unknown result type (might be due to invalid IL or missing references)
			//IL_156b3: Expected O, but got Unknown
			//IL_157ae: Unknown result type (might be due to invalid IL or missing references)
			//IL_157b8: Expected O, but got Unknown
			//IL_1588c: Unknown result type (might be due to invalid IL or missing references)
			//IL_15896: Expected O, but got Unknown
			//IL_15931: Unknown result type (might be due to invalid IL or missing references)
			//IL_1593b: Expected O, but got Unknown
			//IL_15a0f: Unknown result type (might be due to invalid IL or missing references)
			//IL_15a19: Expected O, but got Unknown
			//IL_15ac9: Unknown result type (might be due to invalid IL or missing references)
			//IL_15ad3: Expected O, but got Unknown
			//IL_15b6e: Unknown result type (might be due to invalid IL or missing references)
			//IL_15b78: Expected O, but got Unknown
			//IL_15c4c: Unknown result type (might be due to invalid IL or missing references)
			//IL_15c56: Expected O, but got Unknown
			//IL_15d24: Unknown result type (might be due to invalid IL or missing references)
			//IL_15d2e: Expected O, but got Unknown
			//IL_15e02: Unknown result type (might be due to invalid IL or missing references)
			//IL_15e0c: Expected O, but got Unknown
			//IL_15eda: Unknown result type (might be due to invalid IL or missing references)
			//IL_15ee4: Expected O, but got Unknown
			//IL_15fb2: Unknown result type (might be due to invalid IL or missing references)
			//IL_15fbc: Expected O, but got Unknown
			//IL_16090: Unknown result type (might be due to invalid IL or missing references)
			//IL_1609a: Expected O, but got Unknown
			//IL_16168: Unknown result type (might be due to invalid IL or missing references)
			//IL_16172: Expected O, but got Unknown
			//IL_16246: Unknown result type (might be due to invalid IL or missing references)
			//IL_16250: Expected O, but got Unknown
			//IL_162eb: Unknown result type (might be due to invalid IL or missing references)
			//IL_162f5: Expected O, but got Unknown
			//IL_163de: Unknown result type (might be due to invalid IL or missing references)
			//IL_163e8: Expected O, but got Unknown
			//IL_164b6: Unknown result type (might be due to invalid IL or missing references)
			//IL_164c0: Expected O, but got Unknown
			//IL_16594: Unknown result type (might be due to invalid IL or missing references)
			//IL_1659e: Expected O, but got Unknown
			//IL_168e8: Unknown result type (might be due to invalid IL or missing references)
			//IL_168f2: Expected O, but got Unknown
			//IL_16968: Unknown result type (might be due to invalid IL or missing references)
			//IL_16972: Expected O, but got Unknown
			components = new Container();
			ComponentResourceManager componentResourceManager = new ComponentResourceManager(typeof(Defaults));
			grpHomeLocation = new GroupBox();
			txtAperture = new TextBox();
			label11 = new Label();
			label73 = new Label();
			label72 = new Label();
			label67 = new Label();
			txtSiteName = new TextBox();
			label4 = new Label();
			label3 = new Label();
			txtTimeZone = new TextBox();
			txtAlt = new TextBox();
			txtLatDDD = new TextBox();
			label2 = new Label();
			label1 = new Label();
			txtLongDDD = new TextBox();
			grpURLs = new GroupBox();
			panel4 = new Panel();
			label65 = new Label();
			cmdResetDownloadAddresses = new Button();
			label185 = new Label();
			txtJupSats_Server = new TextBox();
			label181 = new Label();
			txtLuckyStarServer = new TextBox();
			label180 = new Label();
			txt7Timer = new TextBox();
			label166 = new Label();
			txtWDScodesFile = new TextBox();
			txtWDScodesServer = new TextBox();
			label164 = new Label();
			txtRIO_server = new TextBox();
			label106 = new Label();
			label82 = new Label();
			textBox2 = new TextBox();
			label158 = new Label();
			label156 = new Label();
			txtOrb6File = new TextBox();
			txtOrb6Server = new TextBox();
			label155 = new Label();
			txtCALL = new TextBox();
			label153 = new Label();
			txtOccultServer = new TextBox();
			label151 = new Label();
			label150 = new Label();
			label149 = new Label();
			txtEOPpre62_File = new TextBox();
			txtEOPpost62_File = new TextBox();
			txtEOPpre62_Server = new TextBox();
			txtEOPpost62_Server = new TextBox();
			label148 = new Label();
			label147 = new Label();
			txtAstDysFile = new TextBox();
			txtAstDysServer = new TextBox();
			label143 = new Label();
			label142 = new Label();
			txtVSXfile = new TextBox();
			txtVSXserver = new TextBox();
			label140 = new Label();
			label139 = new Label();
			label138 = new Label();
			label137 = new Label();
			txtIFfile = new TextBox();
			txtWDSfile = new TextBox();
			txtIFserver = new TextBox();
			txtWDSserver = new TextBox();
			label136 = new Label();
			label135 = new Label();
			label134 = new Label();
			label116 = new Label();
			label115 = new Label();
			chkPreserveFutureDAT = new CheckBox();
			txtCZMirrorFile = new TextBox();
			txtCZMirrorServer = new TextBox();
			label24 = new Label();
			txtVizierAstorbFile = new TextBox();
			txtVizierAstorbServer = new TextBox();
			label9 = new Label();
			txtFutureAllFile = new TextBox();
			txtFutureAllServer = new TextBox();
			label8 = new Label();
			txtFutureFile = new TextBox();
			txtFutureServer = new TextBox();
			label7 = new Label();
			txtCometFile = new TextBox();
			txtCometServer = new TextBox();
			label6 = new Label();
			txtMPCOrbFile = new TextBox();
			txtMPCOrbServer = new TextBox();
			label5 = new Label();
			txtAstorbFile = new TextBox();
			txtAstorbServer = new TextBox();
			label25 = new Label();
			label128 = new Label();
			numericUpDown57 = new NumericUpDown();
			label127 = new Label();
			label126 = new Label();
			grpEmailAdvanced = new GroupBox();
			cmdHelpInternet = new Button();
			label125 = new Label();
			txtPort = new TextBox();
			checkBox5 = new CheckBox();
			label124 = new Label();
			label123 = new Label();
			label122 = new Label();
			label121 = new Label();
			textBox6 = new TextBox();
			textBox5 = new TextBox();
			label119 = new Label();
			textBox4 = new TextBox();
			label118 = new Label();
			label74 = new Label();
			label10 = new Label();
			FTPPassword = new TextBox();
			label12 = new Label();
			label20 = new Label();
			label19 = new Label();
			label18 = new Label();
			label17 = new Label();
			label13 = new Label();
			label14 = new Label();
			label15 = new Label();
			label16 = new Label();
			grpStarChartInitialise = new GroupBox();
			checkBox7 = new CheckBox();
			chkGaia16Auto = new CheckBox();
			label175 = new Label();
			checkBox8 = new CheckBox();
			checkBox4 = new CheckBox();
			chkGaia = new CheckBox();
			chkGaia12 = new CheckBox();
			label117 = new Label();
			label163 = new Label();
			chkPPMXL = new CheckBox();
			chkAutoPPMXL = new CheckBox();
			chkAutoUCAC4 = new CheckBox();
			chkUCAC4 = new CheckBox();
			chkAutoNomad = new CheckBox();
			cmbAutoSizeDeg = new ComboBox();
			cmbAutoMagLimit = new ComboBox();
			cmbAutoSizePixels = new ComboBox();
			label112 = new Label();
			label110 = new Label();
			label111 = new Label();
			label109 = new Label();
			label26 = new Label();
			chkNomad = new CheckBox();
			label22 = new Label();
			label21 = new Label();
			cmbMag = new ComboBox();
			cmbSize = new ComboBox();
			x1DegToolStripMenuItem = new ToolStripMenuItem();
			x2DegToolStripMenuItem = new ToolStripMenuItem();
			x3DegToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem11 = new ToolStripMenuItem();
			x5DegToolStripMenuItem = new ToolStripMenuItem();
			x6DegToolStripMenuItem = new ToolStripMenuItem();
			x8DegToolStripMenuItem = new ToolStripMenuItem();
			x10DegToolStripMenuItem = new ToolStripMenuItem();
			x12DegToolStripMenuItem = new ToolStripMenuItem();
			x15DegToolStripMenuItem = new ToolStripMenuItem();
			x20DegToolStripMenuItem = new ToolStripMenuItem();
			toolStripMenuItem1 = new ToolStripMenuItem();
			toolStripMenuItem2 = new ToolStripMenuItem();
			toolStripMenuItem3 = new ToolStripMenuItem();
			toolStripMenuItem4 = new ToolStripMenuItem();
			toolStripMenuItem5 = new ToolStripMenuItem();
			toolStripMenuItem6 = new ToolStripMenuItem();
			toolStripMenuItem7 = new ToolStripMenuItem();
			toolStripMenuItem8 = new ToolStripMenuItem();
			toolStripMenuItem9 = new ToolStripMenuItem();
			toolStripMenuItem10 = new ToolStripMenuItem();
			toolStripMenuItem12 = new ToolStripMenuItem();
			panel1 = new Panel();
			grpC2A = new GroupBox();
			pictureBox3 = new PictureBox();
			chkC2A_MultipleInstances = new CheckBox();
			label179 = new Label();
			optUCAC4 = new RadioButton();
			optPPMXL = new RadioButton();
			optNOMAD = new RadioButton();
			optSAO = new RadioButton();
			cmbC2AFieldSize = new ComboBox();
			label146 = new Label();
			label178 = new Label();
			cmdFolder_C2A = new Button();
			txtC2A = new TextBox();
			label177 = new Label();
			linkC2A = new LinkLabel();
			grpEmails = new GroupBox();
			grpTest = new GroupBox();
			chkReposition = new CheckBox();
			cmdPurgeLunarCache = new Button();
			chkForceVSOP87 = new CheckBox();
			grpUpdates = new GroupBox();
			cmbLightCurveReminder = new ComboBox();
			label173 = new Label();
			label145 = new Label();
			cmbUpdateFrequency = new ComboBox();
			label144 = new Label();
			grpAsteroidMultiPathMaps = new GroupBox();
			checkBox3 = new CheckBox();
			label85 = new Label();
			updnWest = new NumericUpDown();
			updnEast = new NumericUpDown();
			updnNorth = new NumericUpDown();
			updnSouth = new NumericUpDown();
			label90 = new Label();
			label91 = new Label();
			label92 = new Label();
			numericUpDown4 = new NumericUpDown();
			label93 = new Label();
			numericUpDown3 = new NumericUpDown();
			label86 = new Label();
			numericUpDown2 = new NumericUpDown();
			label87 = new Label();
			numericUpDown1 = new NumericUpDown();
			label88 = new Label();
			numericUpDown8 = new NumericUpDown();
			label89 = new Label();
			numericUpDown7 = new NumericUpDown();
			numericUpDown56 = new NumericUpDown();
			numericUpDown6 = new NumericUpDown();
			numericUpDown55 = new NumericUpDown();
			numericUpDown5 = new NumericUpDown();
			numericUpDown54 = new NumericUpDown();
			numericUpDown12 = new NumericUpDown();
			numericUpDown53 = new NumericUpDown();
			numericUpDown11 = new NumericUpDown();
			numericUpDown52 = new NumericUpDown();
			numericUpDown10 = new NumericUpDown();
			numericUpDown51 = new NumericUpDown();
			numericUpDown9 = new NumericUpDown();
			numericUpDown50 = new NumericUpDown();
			numericUpDown49 = new NumericUpDown();
			numericUpDown48 = new NumericUpDown();
			numericUpDown47 = new NumericUpDown();
			numericUpDown46 = new NumericUpDown();
			numericUpDown41 = new NumericUpDown();
			numericUpDown45 = new NumericUpDown();
			numericUpDown42 = new NumericUpDown();
			numericUpDown44 = new NumericUpDown();
			numericUpDown43 = new NumericUpDown();
			grpLunarReports = new GroupBox();
			updnEOPReminder = new NumericUpDown();
			label132 = new Label();
			updnCentury = new NumericUpDown();
			label68 = new Label();
			grpAdministrator = new GroupBox();
			chkLunarGlobal = new CheckBox();
			chkLunarRegional = new CheckBox();
			checkBox2 = new CheckBox();
			label97 = new Label();
			grpSiteFiles = new GroupBox();
			label81 = new Label();
			label40 = new Label();
			cmbSortOrder = new ComboBox();
			cmbSitePlot = new ComboBox();
			label39 = new Label();
			updnMagCorrection = new NumericUpDown();
			label38 = new Label();
			label37 = new Label();
			updnTelescopeAperture = new NumericUpDown();
			label36 = new Label();
			updnGrazeTtravelDistance = new NumericUpDown();
			grpFileSaveGraphics = new GroupBox();
			updnFontSize = new NumericUpDown();
			label131 = new Label();
			updnLineThickness = new NumericUpDown();
			label130 = new Label();
			chkSmoothing = new CheckBox();
			label114 = new Label();
			label80 = new Label();
			cmbGraphicSaveType = new ComboBox();
			grpAsteroidSearchDisplay = new GroupBox();
			panel2 = new Panel();
			label183 = new Label();
			updnPlutoDecCorrn = new NumericUpDown();
			updnPlutoRACorrn = new NumericUpDown();
			label71 = new Label();
			label69 = new Label();
			label70 = new Label();
			label79 = new Label();
			label23 = new Label();
			updnDAteRangeInAsteroidASelection = new NumericUpDown();
			chkExportElements = new CheckBox();
			updnMaximumSearchEvents = new NumericUpDown();
			label182 = new Label();
			chkAddSAO = new CheckBox();
			checkBox6 = new CheckBox();
			label113 = new Label();
			cmbAutoFileType = new ComboBox();
			label108 = new Label();
			chkBessel = new CheckBox();
			chkColour = new CheckBox();
			label84 = new Label();
			cmbSiteFiles = new ComboBox();
			updnTitanDiameter = new NumericUpDown();
			label41 = new Label();
			chkHTM = new CheckBox();
			updnTritonDiameter = new NumericUpDown();
			chkGoogleEarth = new CheckBox();
			label78 = new Label();
			chkPrepoint = new CheckBox();
			updnPlutoDiameter = new NumericUpDown();
			chkMultisite = new CheckBox();
			label77 = new Label();
			chkPathCoords = new CheckBox();
			chkStarChart = new CheckBox();
			label63 = new Label();
			chkWorld = new CheckBox();
			updnUranusDiameter = new NumericUpDown();
			chkAutoGenerate = new CheckBox();
			label61 = new Label();
			updnJupiterDiameter = new NumericUpDown();
			updnSaturnDiameter = new NumericUpDown();
			label62 = new Label();
			updnUncertainty = new NumericUpDown();
			label83 = new Label();
			updnNeptuneDiameter = new NumericUpDown();
			updnUranus = new NumericUpDown();
			label60 = new Label();
			label27 = new Label();
			label59 = new Label();
			label28 = new Label();
			label58 = new Label();
			updnMars = new NumericUpDown();
			label57 = new Label();
			label29 = new Label();
			updnJupiter = new NumericUpDown();
			label30 = new Label();
			updnNeptune = new NumericUpDown();
			label31 = new Label();
			label32 = new Label();
			updnPluto = new NumericUpDown();
			label33 = new Label();
			label34 = new Label();
			updnSaturn = new NumericUpDown();
			label35 = new Label();
			updnVenus = new NumericUpDown();
			updnMercury = new NumericUpDown();
			grpGoogleEarth = new GroupBox();
			txtGoogleMapsAPI = new TextBox();
			label133 = new Label();
			numericUpDown59 = new NumericUpDown();
			numericUpDown58 = new NumericUpDown();
			label159 = new Label();
			label157 = new Label();
			label75 = new Label();
			checkBox1 = new CheckBox();
			grpOptionalCatalogues = new GroupBox();
			cmdDownload_u4i_files = new Button();
			label184 = new Label();
			label174 = new Label();
			pictureBox2 = new PictureBox();
			cmdU4XRefs = new Button();
			cmdUCAC4CheckCorrected = new Button();
			pictureBox1 = new PictureBox();
			cmdU4Help = new Button();
			label162 = new Label();
			cmdFolder_UCAC4 = new Button();
			label160 = new Label();
			txtUCAC4 = new TextBox();
			label141 = new Label();
			cmdFolderNOMAD = new Button();
			cmdFolderNomad_Short = new Button();
			label76 = new Label();
			label66 = new Label();
			txtNomadShort = new TextBox();
			label64 = new Label();
			txtNOMAD = new TextBox();
			grpLunarOccultations = new GroupBox();
			label172 = new Label();
			label171 = new Label();
			label170 = new Label();
			label169 = new Label();
			label168 = new Label();
			updnMaxSep = new NumericUpDown();
			updnMinSep = new NumericUpDown();
			chkLunarCoordinates = new CheckBox();
			label129 = new Label();
			chkMultiSiteAltitude = new CheckBox();
			cmbLibrationsReduce = new ComboBox();
			label120 = new Label();
			label105 = new Label();
			label96 = new Label();
			label56 = new Label();
			label95 = new Label();
			label55 = new Label();
			label94 = new Label();
			label54 = new Label();
			label51 = new Label();
			label53 = new Label();
			cmbGrazeProfile_FileType = new ComboBox();
			label52 = new Label();
			numericUpDown37 = new NumericUpDown();
			label50 = new Label();
			numericUpDown38 = new NumericUpDown();
			cmbLibrations = new ComboBox();
			numericUpDown39 = new NumericUpDown();
			label49 = new Label();
			numericUpDown40 = new NumericUpDown();
			label45 = new Label();
			textBox36 = new TextBox();
			label46 = new Label();
			numericUpDown33 = new NumericUpDown();
			updnAlt = new NumericUpDown();
			numericUpDown34 = new NumericUpDown();
			updnStep = new NumericUpDown();
			numericUpDown35 = new NumericUpDown();
			label47 = new Label();
			numericUpDown36 = new NumericUpDown();
			label48 = new Label();
			textBox35 = new TextBox();
			updnEndLong = new NumericUpDown();
			numericUpDown29 = new NumericUpDown();
			updnStartLong = new NumericUpDown();
			numericUpDown30 = new NumericUpDown();
			panel3 = new Panel();
			label107 = new Label();
			optGrazeFormatDDD = new RadioButton();
			label44 = new Label();
			optGrazeFormatDMS = new RadioButton();
			optGrazeFormatDMM = new RadioButton();
			numericUpDown31 = new NumericUpDown();
			chkGrazeProfileKM_Mi = new CheckBox();
			numericUpDown32 = new NumericUpDown();
			label43 = new Label();
			textBox34 = new TextBox();
			updnGraze45 = new NumericUpDown();
			numericUpDown25 = new NumericUpDown();
			label42 = new Label();
			numericUpDown26 = new NumericUpDown();
			updnGraze65 = new NumericUpDown();
			numericUpDown27 = new NumericUpDown();
			textBox32 = new TextBox();
			numericUpDown28 = new NumericUpDown();
			textBox30 = new TextBox();
			textBox33 = new TextBox();
			numericUpDown13 = new NumericUpDown();
			numericUpDown21 = new NumericUpDown();
			numericUpDown14 = new NumericUpDown();
			numericUpDown22 = new NumericUpDown();
			numericUpDown15 = new NumericUpDown();
			numericUpDown23 = new NumericUpDown();
			numericUpDown16 = new NumericUpDown();
			numericUpDown24 = new NumericUpDown();
			textBox31 = new TextBox();
			numericUpDown20 = new NumericUpDown();
			numericUpDown17 = new NumericUpDown();
			numericUpDown19 = new NumericUpDown();
			numericUpDown18 = new NumericUpDown();
			toolTip1 = new ToolTip(components);
			openFileDialog1 = new OpenFileDialog();
			menuStrip1 = new MenuStrip();
			undoSessionChangesToolStripMenuItem = new ToolStripMenuItem();
			resstToDefaultsToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			saveAndExitToolStripMenuItem = new ToolStripMenuItem();
			((Control)grpHomeLocation).SuspendLayout();
			((Control)grpURLs).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((ISupportInitialize)numericUpDown57).BeginInit();
			((Control)grpEmailAdvanced).SuspendLayout();
			((Control)grpStarChartInitialise).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)grpC2A).SuspendLayout();
			((ISupportInitialize)pictureBox3).BeginInit();
			((Control)grpEmails).SuspendLayout();
			((Control)grpTest).SuspendLayout();
			((Control)grpUpdates).SuspendLayout();
			((Control)grpAsteroidMultiPathMaps).SuspendLayout();
			((ISupportInitialize)updnWest).BeginInit();
			((ISupportInitialize)updnEast).BeginInit();
			((ISupportInitialize)updnNorth).BeginInit();
			((ISupportInitialize)updnSouth).BeginInit();
			((ISupportInitialize)numericUpDown4).BeginInit();
			((ISupportInitialize)numericUpDown3).BeginInit();
			((ISupportInitialize)numericUpDown2).BeginInit();
			((ISupportInitialize)numericUpDown1).BeginInit();
			((ISupportInitialize)numericUpDown8).BeginInit();
			((ISupportInitialize)numericUpDown7).BeginInit();
			((ISupportInitialize)numericUpDown56).BeginInit();
			((ISupportInitialize)numericUpDown6).BeginInit();
			((ISupportInitialize)numericUpDown55).BeginInit();
			((ISupportInitialize)numericUpDown5).BeginInit();
			((ISupportInitialize)numericUpDown54).BeginInit();
			((ISupportInitialize)numericUpDown12).BeginInit();
			((ISupportInitialize)numericUpDown53).BeginInit();
			((ISupportInitialize)numericUpDown11).BeginInit();
			((ISupportInitialize)numericUpDown52).BeginInit();
			((ISupportInitialize)numericUpDown10).BeginInit();
			((ISupportInitialize)numericUpDown51).BeginInit();
			((ISupportInitialize)numericUpDown9).BeginInit();
			((ISupportInitialize)numericUpDown50).BeginInit();
			((ISupportInitialize)numericUpDown49).BeginInit();
			((ISupportInitialize)numericUpDown48).BeginInit();
			((ISupportInitialize)numericUpDown47).BeginInit();
			((ISupportInitialize)numericUpDown46).BeginInit();
			((ISupportInitialize)numericUpDown41).BeginInit();
			((ISupportInitialize)numericUpDown45).BeginInit();
			((ISupportInitialize)numericUpDown42).BeginInit();
			((ISupportInitialize)numericUpDown44).BeginInit();
			((ISupportInitialize)numericUpDown43).BeginInit();
			((Control)grpLunarReports).SuspendLayout();
			((ISupportInitialize)updnEOPReminder).BeginInit();
			((ISupportInitialize)updnCentury).BeginInit();
			((Control)grpAdministrator).SuspendLayout();
			((Control)grpSiteFiles).SuspendLayout();
			((ISupportInitialize)updnMagCorrection).BeginInit();
			((ISupportInitialize)updnTelescopeAperture).BeginInit();
			((ISupportInitialize)updnGrazeTtravelDistance).BeginInit();
			((Control)grpFileSaveGraphics).SuspendLayout();
			((ISupportInitialize)updnFontSize).BeginInit();
			((ISupportInitialize)updnLineThickness).BeginInit();
			((Control)grpAsteroidSearchDisplay).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((ISupportInitialize)updnPlutoDecCorrn).BeginInit();
			((ISupportInitialize)updnPlutoRACorrn).BeginInit();
			((ISupportInitialize)updnDAteRangeInAsteroidASelection).BeginInit();
			((ISupportInitialize)updnMaximumSearchEvents).BeginInit();
			((ISupportInitialize)updnTitanDiameter).BeginInit();
			((ISupportInitialize)updnTritonDiameter).BeginInit();
			((ISupportInitialize)updnPlutoDiameter).BeginInit();
			((ISupportInitialize)updnUranusDiameter).BeginInit();
			((ISupportInitialize)updnJupiterDiameter).BeginInit();
			((ISupportInitialize)updnSaturnDiameter).BeginInit();
			((ISupportInitialize)updnUncertainty).BeginInit();
			((ISupportInitialize)updnNeptuneDiameter).BeginInit();
			((ISupportInitialize)updnUranus).BeginInit();
			((ISupportInitialize)updnMars).BeginInit();
			((ISupportInitialize)updnJupiter).BeginInit();
			((ISupportInitialize)updnNeptune).BeginInit();
			((ISupportInitialize)updnPluto).BeginInit();
			((ISupportInitialize)updnSaturn).BeginInit();
			((ISupportInitialize)updnVenus).BeginInit();
			((ISupportInitialize)updnMercury).BeginInit();
			((Control)grpGoogleEarth).SuspendLayout();
			((ISupportInitialize)numericUpDown59).BeginInit();
			((ISupportInitialize)numericUpDown58).BeginInit();
			((Control)grpOptionalCatalogues).SuspendLayout();
			((ISupportInitialize)pictureBox2).BeginInit();
			((ISupportInitialize)pictureBox1).BeginInit();
			((Control)grpLunarOccultations).SuspendLayout();
			((ISupportInitialize)updnMaxSep).BeginInit();
			((ISupportInitialize)updnMinSep).BeginInit();
			((ISupportInitialize)numericUpDown37).BeginInit();
			((ISupportInitialize)numericUpDown38).BeginInit();
			((ISupportInitialize)numericUpDown39).BeginInit();
			((ISupportInitialize)numericUpDown40).BeginInit();
			((ISupportInitialize)numericUpDown33).BeginInit();
			((ISupportInitialize)updnAlt).BeginInit();
			((ISupportInitialize)numericUpDown34).BeginInit();
			((ISupportInitialize)updnStep).BeginInit();
			((ISupportInitialize)numericUpDown35).BeginInit();
			((ISupportInitialize)numericUpDown36).BeginInit();
			((ISupportInitialize)updnEndLong).BeginInit();
			((ISupportInitialize)numericUpDown29).BeginInit();
			((ISupportInitialize)updnStartLong).BeginInit();
			((ISupportInitialize)numericUpDown30).BeginInit();
			((Control)panel3).SuspendLayout();
			((ISupportInitialize)numericUpDown31).BeginInit();
			((ISupportInitialize)numericUpDown32).BeginInit();
			((ISupportInitialize)updnGraze45).BeginInit();
			((ISupportInitialize)numericUpDown25).BeginInit();
			((ISupportInitialize)numericUpDown26).BeginInit();
			((ISupportInitialize)updnGraze65).BeginInit();
			((ISupportInitialize)numericUpDown27).BeginInit();
			((ISupportInitialize)numericUpDown28).BeginInit();
			((ISupportInitialize)numericUpDown13).BeginInit();
			((ISupportInitialize)numericUpDown21).BeginInit();
			((ISupportInitialize)numericUpDown14).BeginInit();
			((ISupportInitialize)numericUpDown22).BeginInit();
			((ISupportInitialize)numericUpDown15).BeginInit();
			((ISupportInitialize)numericUpDown23).BeginInit();
			((ISupportInitialize)numericUpDown16).BeginInit();
			((ISupportInitialize)numericUpDown24).BeginInit();
			((ISupportInitialize)numericUpDown20).BeginInit();
			((ISupportInitialize)numericUpDown17).BeginInit();
			((ISupportInitialize)numericUpDown19).BeginInit();
			((ISupportInitialize)numericUpDown18).BeginInit();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)grpHomeLocation).set_BackColor(Color.AntiqueWhite);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)txtAperture);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label11);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label73);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label72);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label67);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)txtSiteName);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label4);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label3);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)txtTimeZone);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)txtAlt);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)txtLatDDD);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label2);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)label1);
			((Control)grpHomeLocation).get_Controls().Add((Control)(object)txtLongDDD);
			((Control)grpHomeLocation).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpHomeLocation).set_Location(new Point(13, 17));
			((Control)grpHomeLocation).set_Name("grpHomeLocation");
			((Control)grpHomeLocation).set_Size(new Size(758, 127));
			((Control)grpHomeLocation).set_TabIndex(0);
			grpHomeLocation.set_TabStop(false);
			((Control)grpHomeLocation).set_Text("1. Site details  -  low precision");
			((Control)txtAperture).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Site_Aperture", true, (DataSourceUpdateMode)1));
			((Control)txtAperture).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAperture).set_Location(new Point(359, 98));
			((Control)txtAperture).set_Name("txtAperture");
			((Control)txtAperture).set_Size(new Size(33, 20));
			((Control)txtAperture).set_TabIndex(13);
			((Control)txtAperture).set_Text(Settings.Default.Site_Aperture);
			((Control)label11).set_AutoSize(true);
			((Control)label11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label11).set_Location(new Point(272, 102));
			((Control)label11).set_Name("label11");
			((Control)label11).set_Size(new Size(70, 13));
			((Control)label11).set_TabIndex(12);
			((Control)label11).set_Text("Aperture (cm)");
			((Control)label73).set_AutoSize(true);
			((Control)label73).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label73).set_Location(new Point(341, 50));
			((Control)label73).set_Name("label73");
			((Control)label73).set_Size(new Size(151, 12));
			((Control)label73).set_TabIndex(3);
			((Control)label73).set_Text("Do not use any of   /  :  *  #  ?  \"  <  >  | \r\n");
			((Control)label72).set_AutoSize(true);
			((Control)label72).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label72).set_Location(new Point(64, 26));
			((Control)label72).set_Name("label72");
			((Control)label72).set_Size(new Size(600, 13));
			((Control)label72).set_TabIndex(0);
			((Control)label72).set_Text("For some predictions, Occult uses a low-precision site location. Enter here the site coordinates to be used as the default value.");
			((Control)label67).set_AutoSize(true);
			((Control)label67).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label67).set_Location(new Point(80, 49));
			((Control)label67).set_Name("label67");
			((Control)label67).set_Size(new Size(54, 13));
			((Control)label67).set_TabIndex(1);
			((Control)label67).set_Text("Site name");
			((Control)txtSiteName).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Site_Name", true, (DataSourceUpdateMode)1));
			((Control)txtSiteName).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtSiteName).set_Location(new Point(146, 46));
			((Control)txtSiteName).set_Name("txtSiteName");
			((Control)txtSiteName).set_Size(new Size(189, 20));
			((Control)txtSiteName).set_TabIndex(2);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(88, 103));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(58, 13));
			((Control)label4).set_TabIndex(10);
			((Control)label4).set_Text("Time Zone");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(490, 76));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(85, 13));
			((Control)label3).set_TabIndex(8);
			((Control)label3).set_Text("Altitude  [metres]");
			((Control)txtTimeZone).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Site_TimeZone_Hrs", true, (DataSourceUpdateMode)1));
			((Control)txtTimeZone).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtTimeZone).set_Location(new Point(150, 99));
			((Control)txtTimeZone).set_Name("txtTimeZone");
			((Control)txtTimeZone).set_Size(new Size(38, 20));
			((Control)txtTimeZone).set_TabIndex(11);
			((Control)txtTimeZone).set_Text(Settings.Default.Site_TimeZone_Hrs);
			((Control)txtAlt).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Site_Altitude", true, (DataSourceUpdateMode)1));
			((Control)txtAlt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt).set_Location(new Point(582, 72));
			((Control)txtAlt).set_Name("txtAlt");
			((Control)txtAlt).set_Size(new Size(56, 20));
			((Control)txtAlt).set_TabIndex(9);
			((Control)txtLatDDD).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Site_Latitude_dd_d", true, (DataSourceUpdateMode)1));
			((Control)txtLatDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDDD).set_Location(new Point(359, 72));
			((Control)txtLatDDD).set_Name("txtLatDDD");
			((Control)txtLatDDD).set_Size(new Size(66, 20));
			((Control)txtLatDDD).set_TabIndex(7);
			((Control)txtLatDDD).add_Leave((EventHandler)txtLatDDD_Leave);
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label2).set_Location(new Point(252, 76));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(90, 13));
			((Control)label2).set_TabIndex(6);
			((Control)label2).set_Text("Latitude    [d.ddd]");
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(17, 76));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(123, 13));
			((Control)label1).set_TabIndex(4);
			((Control)label1).set_Text("East Longitude    [d.ddd]");
			((Control)txtLongDDD).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Site_Longitude_dd_d", true, (DataSourceUpdateMode)1));
			((Control)txtLongDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDDD).set_Location(new Point(151, 72));
			((Control)txtLongDDD).set_Name("txtLongDDD");
			((Control)txtLongDDD).set_Size(new Size(67, 20));
			((Control)txtLongDDD).set_TabIndex(5);
			((Control)txtLongDDD).add_TextChanged((EventHandler)txtLongDDD_TextChanged);
			((Control)txtLongDDD).add_Leave((EventHandler)txtLongDDD_Leave);
			((Control)grpURLs).set_BackColor(Color.PaleGoldenrod);
			((Control)grpURLs).get_Controls().Add((Control)(object)panel4);
			((Control)grpURLs).get_Controls().Add((Control)(object)label185);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtJupSats_Server);
			((Control)grpURLs).get_Controls().Add((Control)(object)label181);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtLuckyStarServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label180);
			((Control)grpURLs).get_Controls().Add((Control)(object)txt7Timer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label166);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtWDScodesFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtWDScodesServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label164);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtRIO_server);
			((Control)grpURLs).get_Controls().Add((Control)(object)label106);
			((Control)grpURLs).get_Controls().Add((Control)(object)label82);
			((Control)grpURLs).get_Controls().Add((Control)(object)textBox2);
			((Control)grpURLs).get_Controls().Add((Control)(object)label158);
			((Control)grpURLs).get_Controls().Add((Control)(object)label156);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtOrb6File);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtOrb6Server);
			((Control)grpURLs).get_Controls().Add((Control)(object)label155);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtCALL);
			((Control)grpURLs).get_Controls().Add((Control)(object)label153);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtOccultServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label151);
			((Control)grpURLs).get_Controls().Add((Control)(object)label150);
			((Control)grpURLs).get_Controls().Add((Control)(object)label149);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtEOPpre62_File);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtEOPpost62_File);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtEOPpre62_Server);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtEOPpost62_Server);
			((Control)grpURLs).get_Controls().Add((Control)(object)label148);
			((Control)grpURLs).get_Controls().Add((Control)(object)label147);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtAstDysFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtAstDysServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label143);
			((Control)grpURLs).get_Controls().Add((Control)(object)label142);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtVSXfile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtVSXserver);
			((Control)grpURLs).get_Controls().Add((Control)(object)label140);
			((Control)grpURLs).get_Controls().Add((Control)(object)label139);
			((Control)grpURLs).get_Controls().Add((Control)(object)label138);
			((Control)grpURLs).get_Controls().Add((Control)(object)label137);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtIFfile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtWDSfile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtIFserver);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtWDSserver);
			((Control)grpURLs).get_Controls().Add((Control)(object)label136);
			((Control)grpURLs).get_Controls().Add((Control)(object)label135);
			((Control)grpURLs).get_Controls().Add((Control)(object)label134);
			((Control)grpURLs).get_Controls().Add((Control)(object)label116);
			((Control)grpURLs).get_Controls().Add((Control)(object)label115);
			((Control)grpURLs).get_Controls().Add((Control)(object)chkPreserveFutureDAT);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtCZMirrorFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtCZMirrorServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label24);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtVizierAstorbFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtVizierAstorbServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label9);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtFutureAllFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtFutureAllServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label8);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtFutureFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtFutureServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label7);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtCometFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtCometServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label6);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtMPCOrbFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtMPCOrbServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label5);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtAstorbFile);
			((Control)grpURLs).get_Controls().Add((Control)(object)txtAstorbServer);
			((Control)grpURLs).get_Controls().Add((Control)(object)label25);
			((Control)grpURLs).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpURLs).set_Location(new Point(13, 747));
			((Control)grpURLs).set_Name("grpURLs");
			((Control)grpURLs).set_Size(new Size(758, 518));
			((Control)grpURLs).set_TabIndex(3);
			grpURLs.set_TabStop(false);
			((Control)grpURLs).set_Text("5. Download addresses");
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)label65);
			((Control)panel4).get_Controls().Add((Control)(object)cmdResetDownloadAddresses);
			((Control)panel4).set_Location(new Point(389, 453));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(354, 53));
			((Control)panel4).set_TabIndex(89);
			((Control)label65).set_AutoSize(true);
			((Control)label65).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label65).set_Location(new Point(104, 6));
			((Control)label65).set_Name("label65");
			((Control)label65).set_Size(new Size(245, 39));
			((Control)label65).set_TabIndex(88);
			((Control)label65).set_Text("Use this when downloads fail. Typical reasons are:\r\na. The automatic update of an address has failed\r\nb. You have manually changed an address");
			((Control)cmdResetDownloadAddresses).set_BackColor(Color.RoyalBlue);
			((Control)cmdResetDownloadAddresses).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdResetDownloadAddresses).set_ForeColor(Color.Yellow);
			((Control)cmdResetDownloadAddresses).set_Location(new Point(3, 5));
			((Control)cmdResetDownloadAddresses).set_Name("cmdResetDownloadAddresses");
			((Control)cmdResetDownloadAddresses).set_Size(new Size(94, 40));
			((Control)cmdResetDownloadAddresses).set_TabIndex(82);
			((Control)cmdResetDownloadAddresses).set_Text("Update addresses");
			((ButtonBase)cmdResetDownloadAddresses).set_UseVisualStyleBackColor(false);
			((Control)cmdResetDownloadAddresses).add_Click((EventHandler)cmdResetDownloadAddresses_Click);
			((Control)label185).set_AutoSize(true);
			((Control)label185).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label185).set_Location(new Point(5, 341));
			((Control)label185).set_Name("label185");
			((Control)label185).set_Size(new Size(62, 13));
			((Control)label185).set_TabIndex(87);
			((Control)label185).set_Text("Jupiter Sats");
			((Control)txtJupSats_Server).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "JupSats_Server", true, (DataSourceUpdateMode)1));
			((Control)txtJupSats_Server).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtJupSats_Server).set_Location(new Point(69, 338));
			((Control)txtJupSats_Server).set_Name("txtJupSats_Server");
			((Control)txtJupSats_Server).set_Size(new Size(297, 20));
			((Control)txtJupSats_Server).set_TabIndex(86);
			((Control)txtJupSats_Server).set_Text(Settings.Default.JupSats_Server);
			((Control)label181).set_AutoSize(true);
			((Control)label181).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label181).set_Location(new Point(133, 179));
			((Control)label181).set_Name("label181");
			((Control)label181).set_Size(new Size(127, 13));
			((Control)label181).set_TabIndex(85);
			((Control)label181).set_Text("[numbered asteroids only]");
			((Control)txtLuckyStarServer).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "LuckyStar", true, (DataSourceUpdateMode)1));
			((Control)txtLuckyStarServer).set_Font(new Font("Microsoft Sans Serif", 8.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLuckyStarServer).set_Location(new Point(69, 362));
			((Control)txtLuckyStarServer).set_Name("txtLuckyStarServer");
			((Control)txtLuckyStarServer).set_Size(new Size(269, 20));
			((Control)txtLuckyStarServer).set_TabIndex(84);
			((Control)txtLuckyStarServer).set_Text(Settings.Default.LuckyStar);
			((Control)label180).set_AutoSize(true);
			((Control)label180).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label180).set_Location(new Point(12, 360));
			((Control)label180).set_Name("label180");
			((Control)label180).set_Size(new Size(55, 26));
			((Control)label180).set_TabIndex(83);
			((Control)label180).set_Text("LuckyStar\r\n[LESIA]");
			((Control)txt7Timer).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "SevenTimer", true, (DataSourceUpdateMode)1));
			((Control)txt7Timer).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txt7Timer).set_Location(new Point(69, 474));
			((Control)txt7Timer).set_Name("txt7Timer");
			((Control)txt7Timer).set_Size(new Size(204, 20));
			((Control)txt7Timer).set_TabIndex(49);
			((Control)txt7Timer).set_Text(Settings.Default.SevenTimer);
			((Control)label166).set_AutoSize(true);
			((Control)label166).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label166).set_Location(new Point(385, 174));
			((Control)label166).set_Name("label166");
			((Control)label166).set_Size(new Size(65, 13));
			((Control)label166).set_TabIndex(60);
			((Control)label166).set_Text("WDS codes");
			((Control)txtWDScodesFile).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "WDSCodesFile", true, (DataSourceUpdateMode)1));
			((Control)txtWDScodesFile).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtWDScodesFile).set_Location(new Point(662, 170));
			((Control)txtWDScodesFile).set_Name("txtWDScodesFile");
			((Control)txtWDScodesFile).set_Size(new Size(93, 20));
			((Control)txtWDScodesFile).set_TabIndex(62);
			((Control)txtWDScodesFile).set_Text(Settings.Default.WDSCodesFile);
			((Control)txtWDScodesServer).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "WDSDownloadServer", true, (DataSourceUpdateMode)1));
			((Control)txtWDScodesServer).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtWDScodesServer).set_Location(new Point(452, 170));
			((Control)txtWDScodesServer).set_Name("txtWDScodesServer");
			((Control)txtWDScodesServer).set_Size(new Size(204, 20));
			((Control)txtWDScodesServer).set_TabIndex(61);
			((Control)txtWDScodesServer).set_Text(Settings.Default.WDSDownloadServer);
			((Control)label164).set_AutoSize(true);
			((Control)label164).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label164).set_Location(new Point(2, 317));
			((Control)label164).set_Name("label164");
			((Control)label164).set_Size(new Size(65, 13));
			((Control)label164).set_TabIndex(38);
			((Control)label164).set_Text("TNO's [RIO]");
			((Control)txtRIO_server).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "RIO_server", true, (DataSourceUpdateMode)1));
			((Control)txtRIO_server).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtRIO_server).set_Location(new Point(69, 314));
			((Control)txtRIO_server).set_Name("txtRIO_server");
			((Control)txtRIO_server).set_Size(new Size(269, 20));
			((Control)txtRIO_server).set_TabIndex(39);
			((Control)txtRIO_server).set_Text(Settings.Default.RIO_server);
			((Control)txtRIO_server).add_Leave((EventHandler)txtRIO_server_Leave);
			((Control)label106).set_AutoSize(true);
			((Control)label106).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label106).set_Location(new Point(28, 477));
			((Control)label106).set_Name("label106");
			((Control)label106).set_Size(new Size(39, 13));
			((Control)label106).set_TabIndex(48);
			((Control)label106).set_Text("7Timer");
			((Control)label82).set_AutoSize(true);
			((Control)label82).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label82).set_Location(new Point(68, 459));
			((Control)label82).set_Name("label82");
			((Control)label82).set_Size(new Size(55, 13));
			((Control)label82).set_TabIndex(47);
			((Control)label82).set_Text("Weather");
			((Control)textBox2).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "CDS_portal", true, (DataSourceUpdateMode)1));
			((Control)textBox2).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox2).set_Location(new Point(453, 421));
			((Control)textBox2).set_Name("textBox2");
			((Control)textBox2).set_Size(new Size(204, 20));
			((Control)textBox2).set_TabIndex(81);
			((Control)textBox2).set_Text(Settings.Default.CDS_portal);
			((Control)label158).set_AutoSize(true);
			((Control)label158).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label158).set_Location(new Point(451, 408));
			((Control)label158).set_Name("label158");
			((Control)label158).set_Size(new Size(69, 13));
			((Control)label158).set_TabIndex(80);
			((Control)label158).set_Text("CDS Portal");
			((Control)label156).set_AutoSize(true);
			((Control)label156).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label156).set_Location(new Point(396, 218));
			((Control)label156).set_Name("label156");
			((Control)label156).set_Size(new Size(56, 13));
			((Control)label156).set_TabIndex(66);
			((Control)label156).set_Text("6th Orbit...");
			((Control)txtOrb6File).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Orb6DownloadFile", true, (DataSourceUpdateMode)1));
			((Control)txtOrb6File).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtOrb6File).set_Location(new Point(662, 215));
			((Control)txtOrb6File).set_Name("txtOrb6File");
			((Control)txtOrb6File).set_Size(new Size(93, 20));
			((Control)txtOrb6File).set_TabIndex(68);
			((Control)txtOrb6File).set_Text(Settings.Default.Orb6DownloadFile);
			((Control)txtOrb6Server).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Orb6DownloadServer", true, (DataSourceUpdateMode)1));
			((Control)txtOrb6Server).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtOrb6Server).set_Location(new Point(452, 215));
			((Control)txtOrb6Server).set_Name("txtOrb6Server");
			((Control)txtOrb6Server).set_Size(new Size(204, 20));
			((Control)txtOrb6Server).set_TabIndex(67);
			((Control)txtOrb6Server).set_Text(Settings.Default.Orb6DownloadServer);
			((Control)label155).set_AutoSize(true);
			((Control)label155).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label155).set_Location(new Point(36, 425));
			((Control)label155).set_Name("label155");
			((Control)label155).set_Size(new Size(33, 13));
			((Control)label155).set_TabIndex(45);
			((Control)label155).set_Text("CALL");
			((Control)txtCALL).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AsteroidLightCurveData_url", true, (DataSourceUpdateMode)1));
			((Control)txtCALL).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCALL).set_Location(new Point(69, 422));
			((Control)txtCALL).set_Name("txtCALL");
			((Control)txtCALL).set_Size(new Size(288, 20));
			((Control)txtCALL).set_TabIndex(46);
			((Control)txtCALL).set_Text(Settings.Default.AsteroidLightCurveData_url);
			((Control)txtCALL).add_Leave((EventHandler)txtCALL_Leave);
			((Control)label153).set_AutoSize(true);
			((Control)label153).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label153).set_Location(new Point(68, 407));
			((Control)label153).set_Name("label153");
			((Control)label153).set_Size(new Size(157, 13));
			((Control)label153).set_TabIndex(44);
			((Control)label153).set_Text("CALL Asteroid light curves");
			((Control)txtOccultServer).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "OccultServer", true, (DataSourceUpdateMode)1));
			((Control)txtOccultServer).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtOccultServer).set_Location(new Point(452, 57));
			((Control)txtOccultServer).set_Name("txtOccultServer");
			txtOccultServer.set_PasswordChar('/');
			((Control)txtOccultServer).set_Size(new Size(204, 20));
			((Control)txtOccultServer).set_TabIndex(51);
			((Control)txtOccultServer).set_Text(Settings.Default.OccultServer);
			((Control)txtOccultServer).add_Leave((EventHandler)txtOccultServer_Leave);
			((Control)label151).set_AutoSize(true);
			((Control)label151).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label151).set_Location(new Point(450, 42));
			((Control)label151).set_Name("label151");
			((Control)label151).set_Size(new Size(153, 13));
			((Control)label151).set_TabIndex(50);
			((Control)label151).set_Text("OCCULT download server");
			((Control)label150).set_AutoSize(true);
			((Control)label150).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label150).set_Location(new Point(384, 367));
			((Control)label150).set_Name("label150");
			((Control)label150).set_Size(new Size(64, 13));
			((Control)label150).set_TabIndex(77);
			((Control)label150).set_Text("before 1963");
			((Control)label149).set_AutoSize(true);
			((Control)label149).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label149).set_Location(new Point(374, 346));
			((Control)label149).set_Name("label149");
			((Control)label149).set_Size(new Size(74, 13));
			((Control)label149).set_TabIndex(74);
			((Control)label149).set_Text("'63 => present");
			((Control)txtEOPpre62_File).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EOPpre62_File", true, (DataSourceUpdateMode)1));
			((Control)txtEOPpre62_File).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEOPpre62_File).set_Location(new Point(659, 364));
			((Control)txtEOPpre62_File).set_Name("txtEOPpre62_File");
			((Control)txtEOPpre62_File).set_Size(new Size(93, 20));
			((Control)txtEOPpre62_File).set_TabIndex(79);
			((Control)txtEOPpre62_File).set_Text(Settings.Default.EOPpre62_File);
			((Control)txtEOPpre62_File).add_Leave((EventHandler)txtEOPpre62_File_Leave);
			((Control)txtEOPpost62_File).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EOPpost62_File", true, (DataSourceUpdateMode)1));
			((Control)txtEOPpost62_File).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEOPpost62_File).set_Location(new Point(658, 343));
			((Control)txtEOPpost62_File).set_Name("txtEOPpost62_File");
			((Control)txtEOPpost62_File).set_Size(new Size(93, 20));
			((Control)txtEOPpost62_File).set_TabIndex(76);
			((Control)txtEOPpost62_File).set_Text(Settings.Default.EOPpost62_File);
			((Control)txtEOPpost62_File).add_Leave((EventHandler)txtEOPpost62_File_Leave);
			((Control)txtEOPpre62_Server).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EOPpre62_Server", true, (DataSourceUpdateMode)1));
			((Control)txtEOPpre62_Server).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEOPpre62_Server).set_Location(new Point(448, 364));
			((Control)txtEOPpre62_Server).set_Name("txtEOPpre62_Server");
			((Control)txtEOPpre62_Server).set_Size(new Size(204, 20));
			((Control)txtEOPpre62_Server).set_TabIndex(78);
			((Control)txtEOPpre62_Server).set_Text(Settings.Default.EOPpre62_Server);
			((Control)txtEOPpre62_Server).add_Leave((EventHandler)txtEOPpre62_Server_Leave);
			((Control)txtEOPpost62_Server).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EOPpost62_Server", true, (DataSourceUpdateMode)1));
			((Control)txtEOPpost62_Server).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtEOPpost62_Server).set_Location(new Point(448, 343));
			((Control)txtEOPpost62_Server).set_Name("txtEOPpost62_Server");
			((Control)txtEOPpost62_Server).set_Size(new Size(204, 20));
			((Control)txtEOPpost62_Server).set_TabIndex(75);
			((Control)txtEOPpost62_Server).set_Text(Settings.Default.EOPpost62_Server);
			((Control)txtEOPpost62_Server).add_Leave((EventHandler)txtEOPpost62_Server_Leave);
			((Control)label148).set_AutoSize(true);
			((Control)label148).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label148).set_Location(new Point(449, 328));
			((Control)label148).set_Name("label148");
			((Control)label148).set_Size(new Size(170, 13));
			((Control)label148).set_TabIndex(73);
			((Control)label148).set_Text("Earth Orientation Parameters");
			((Control)label147).set_AutoSize(true);
			((Control)label147).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label147).set_ForeColor(Color.DarkRed);
			((Control)label147).set_Location(new Point(7, 22));
			((Control)label147).set_Name("label147");
			((Control)label147).set_Size(new Size(722, 13));
			((Control)label147).set_TabIndex(12);
			((Control)label147).set_Text("The following are default addresses for a range of downloads. They should not be changed without clear instruction to do so..");
			label147.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtAstDysFile).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AstDys2_AllNumFile", true, (DataSourceUpdateMode)1));
			((Control)txtAstDysFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAstDysFile).set_Location(new Point(279, 195));
			((Control)txtAstDysFile).set_Name("txtAstDysFile");
			((Control)txtAstDysFile).set_Size(new Size(93, 20));
			((Control)txtAstDysFile).set_TabIndex(30);
			((Control)txtAstDysFile).set_Text(Settings.Default.AstDys2_AllNumFile);
			((Control)txtAstDysFile).add_Leave((EventHandler)txtAstDysFile_Leave);
			((Control)txtAstDysServer).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AstDys2_Server", true, (DataSourceUpdateMode)1));
			((Control)txtAstDysServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAstDysServer).set_Location(new Point(69, 194));
			((Control)txtAstDysServer).set_Name("txtAstDysServer");
			((Control)txtAstDysServer).set_Size(new Size(204, 20));
			((Control)txtAstDysServer).set_TabIndex(29);
			((Control)txtAstDysServer).set_Text(Settings.Default.AstDys2_Server);
			((Control)txtAstDysServer).add_Leave((EventHandler)txtAstDysServer_Leave);
			((Control)label143).set_AutoSize(true);
			((Control)label143).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label143).set_Location(new Point(12, 191));
			((Control)label143).set_Name("label143");
			((Control)label143).set_Size(new Size(55, 26));
			((Control)label143).set_TabIndex(28);
			((Control)label143).set_Text("Single-line\r\nelements");
			((Control)label142).set_AutoSize(true);
			((Control)label142).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label142).set_Location(new Point(66, 179));
			((Control)label142).set_Name("label142");
			((Control)label142).set_Size(new Size(59, 13));
			((Control)label142).set_TabIndex(27);
			((Control)label142).set_Text("AstDyS-2");
			((Control)txtVSXfile).set_BackColor(SystemColors.Window);
			((Control)txtVSXfile).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AAVSOdownloadFile", true, (DataSourceUpdateMode)1));
			((Control)txtVSXfile).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtVSXfile).set_Location(new Point(659, 279));
			((Control)txtVSXfile).set_Name("txtVSXfile");
			((Control)txtVSXfile).set_Size(new Size(93, 20));
			((Control)txtVSXfile).set_TabIndex(72);
			((Control)txtVSXfile).set_Text(Settings.Default.AAVSOdownloadFile);
			((Control)txtVSXfile).add_Leave((EventHandler)txtVSXfile_Leave);
			((Control)txtVSXserver).set_BackColor(SystemColors.Window);
			((Control)txtVSXserver).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "AAVSOdownloadServer", true, (DataSourceUpdateMode)1));
			((Control)txtVSXserver).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtVSXserver).set_Location(new Point(449, 279));
			((Control)txtVSXserver).set_Name("txtVSXserver");
			((Control)txtVSXserver).set_Size(new Size(204, 20));
			((Control)txtVSXserver).set_TabIndex(71);
			((Control)txtVSXserver).set_Text(Settings.Default.AAVSOdownloadServer);
			((Control)txtVSXserver).add_Leave((EventHandler)txtVSXserver_Leave);
			((Control)label140).set_AutoSize(true);
			((Control)label140).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label140).set_Location(new Point(377, 283));
			((Control)label140).set_Name("label140");
			((Control)label140).set_Size(new Size(72, 13));
			((Control)label140).set_TabIndex(70);
			((Control)label140).set_Text("AAVSO Index");
			((Control)label139).set_AutoSize(true);
			((Control)label139).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label139).set_Location(new Point(450, 264));
			((Control)label139).set_Name("label139");
			((Control)label139).set_Size(new Size(84, 13));
			((Control)label139).set_TabIndex(69);
			((Control)label139).set_Text("Variable stars");
			((Control)label138).set_AutoSize(true);
			((Control)label138).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label138).set_Location(new Point(66, 232));
			((Control)label138).set_Name("label138");
			((Control)label138).set_Size(new Size(174, 13));
			((Control)label138).set_TabIndex(31);
			((Control)label138).set_Text("Asteroid occultation elements");
			((Control)label137).set_AutoSize(true);
			((Control)label137).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label137).set_Location(new Point(453, 83));
			((Control)label137).set_Name("label137");
			((Control)label137).set_Size(new Size(42, 13));
			((Control)label137).set_TabIndex(52);
			((Control)label137).set_Text("Comet");
			((Control)txtIFfile).set_BackColor(SystemColors.Window);
			((Control)txtIFfile).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "IFdownloadFile", true, (DataSourceUpdateMode)1));
			((Control)txtIFfile).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtIFfile).set_Location(new Point(662, 192));
			((Control)txtIFfile).set_Name("txtIFfile");
			((Control)txtIFfile).set_Size(new Size(93, 20));
			((Control)txtIFfile).set_TabIndex(65);
			((Control)txtIFfile).set_Text(Settings.Default.IFdownloadFile);
			((Control)txtIFfile).add_Leave((EventHandler)txtIFfile_Leave);
			((Control)txtWDSfile).set_BackColor(SystemColors.Window);
			((Control)txtWDSfile).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "WDSDownload_File", true, (DataSourceUpdateMode)1));
			((Control)txtWDSfile).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtWDSfile).set_Location(new Point(662, 148));
			((Control)txtWDSfile).set_Name("txtWDSfile");
			((Control)txtWDSfile).set_Size(new Size(93, 20));
			((Control)txtWDSfile).set_TabIndex(59);
			((Control)txtWDSfile).set_Text(Settings.Default.WDSDownload_File);
			((Control)txtWDSfile).add_Leave((EventHandler)txtWDSfile_Leave);
			((Control)txtIFserver).set_BackColor(SystemColors.Window);
			((Control)txtIFserver).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "IFdownloadServer", true, (DataSourceUpdateMode)1));
			((Control)txtIFserver).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtIFserver).set_Location(new Point(452, 192));
			((Control)txtIFserver).set_Name("txtIFserver");
			((Control)txtIFserver).set_Size(new Size(204, 20));
			((Control)txtIFserver).set_TabIndex(64);
			((Control)txtIFserver).set_Text(Settings.Default.IFdownloadServer);
			((Control)txtIFserver).add_Leave((EventHandler)txtIFserver_Leave);
			((Control)txtWDSserver).set_BackColor(SystemColors.Window);
			((Control)txtWDSserver).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "WDSDownloadServer", true, (DataSourceUpdateMode)1));
			((Control)txtWDSserver).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtWDSserver).set_Location(new Point(452, 149));
			((Control)txtWDSserver).set_Name("txtWDSserver");
			((Control)txtWDSserver).set_Size(new Size(204, 20));
			((Control)txtWDSserver).set_TabIndex(58);
			((Control)txtWDSserver).set_Text(Settings.Default.WDSDownloadServer);
			((Control)txtWDSserver).add_Leave((EventHandler)txtWDSserver_Leave);
			((Control)label136).set_AutoSize(true);
			((Control)label136).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label136).set_Location(new Point(419, 153));
			((Control)label136).set_Name("label136");
			((Control)label136).set_Size(new Size(33, 13));
			((Control)label136).set_TabIndex(57);
			((Control)label136).set_Text("WDS");
			((Control)label135).set_AutoSize(true);
			((Control)label135).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label135).set_Location(new Point(382, 196));
			((Control)label135).set_Name("label135");
			((Control)label135).set_Size(new Size(70, 13));
			((Control)label135).set_TabIndex(63);
			((Control)label135).set_Text("4th Interfero..");
			((Control)label134).set_AutoSize(true);
			((Control)label134).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label134).set_Location(new Point(453, 133));
			((Control)label134).set_Name("label134");
			((Control)label134).set_Size(new Size(78, 13));
			((Control)label134).set_TabIndex(56);
			((Control)label134).set_Text("Double stars");
			((Control)label116).set_AutoSize(true);
			((Control)label116).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label116).set_Location(new Point(5, 60));
			((Control)label116).set_Name("label116");
			((Control)label116).set_Size(new Size(62, 13));
			((Control)label116).set_TabIndex(14);
			((Control)label116).set_Text("Lowell Obs.");
			((Control)label115).set_AutoSize(true);
			((Control)label115).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label115).set_Location(new Point(28, 129));
			((Control)label115).set_Name("label115");
			((Control)label115).set_Size(new Size(39, 13));
			((Control)label115).set_TabIndex(21);
			((Control)label115).set_Text("M.P.C.");
			((Control)chkPreserveFutureDAT).set_AutoSize(true);
			chkPreserveFutureDAT.set_Checked(Settings.Default.PreserveFuture_dat);
			((Control)chkPreserveFutureDAT).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "PreserveFuture_dat", true, (DataSourceUpdateMode)1));
			((Control)chkPreserveFutureDAT).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPreserveFutureDAT).set_Location(new Point(34, 291));
			((Control)chkPreserveFutureDAT).set_Name("chkPreserveFutureDAT");
			((Control)chkPreserveFutureDAT).set_Size(new Size(327, 16));
			((Control)chkPreserveFutureDAT).set_TabIndex(40);
			((Control)chkPreserveFutureDAT).set_Text("On download, preserve previous version of Future.dat  && FutureAll540.dat");
			((ButtonBase)chkPreserveFutureDAT).set_UseVisualStyleBackColor(true);
			((Control)txtCZMirrorFile).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "MPCOrb_Mirror_file", true, (DataSourceUpdateMode)1));
			((Control)txtCZMirrorFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCZMirrorFile).set_Location(new Point(279, 147));
			((Control)txtCZMirrorFile).set_Name("txtCZMirrorFile");
			((Control)txtCZMirrorFile).set_Size(new Size(93, 20));
			((Control)txtCZMirrorFile).set_TabIndex(26);
			((Control)txtCZMirrorFile).set_Text(Settings.Default.MPCOrb_Mirror_file);
			((Control)txtCZMirrorFile).add_TextChanged((EventHandler)txtCZMirrorFile_TextChanged);
			((Control)txtCZMirrorServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCZMirrorServer).set_Location(new Point(69, 147));
			((Control)txtCZMirrorServer).set_Name("txtCZMirrorServer");
			((Control)txtCZMirrorServer).set_Size(new Size(204, 20));
			((Control)txtCZMirrorServer).set_TabIndex(25);
			((Control)txtCZMirrorServer).set_Text(Settings.Default.MPCORB_Mirror_Server);
			((Control)txtCZMirrorServer).add_Leave((EventHandler)txtCZMirrorServer_Leave);
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label24).set_Location(new Point(35, 83));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(32, 13));
			((Control)label24).set_TabIndex(17);
			((Control)label24).set_Text("Vizier");
			((Control)txtVizierAstorbFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtVizierAstorbFile).set_Location(new Point(279, 79));
			((Control)txtVizierAstorbFile).set_Name("txtVizierAstorbFile");
			((Control)txtVizierAstorbFile).set_Size(new Size(93, 20));
			((Control)txtVizierAstorbFile).set_TabIndex(19);
			((Control)txtVizierAstorbFile).set_Text(Settings.Default.ASTORB_Mirror_file);
			((Control)txtVizierAstorbFile).add_Leave((EventHandler)txtVizierAstorbFile_Leave);
			((Control)txtVizierAstorbServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtVizierAstorbServer).set_Location(new Point(69, 79));
			((Control)txtVizierAstorbServer).set_Name("txtVizierAstorbServer");
			((Control)txtVizierAstorbServer).set_Size(new Size(204, 20));
			((Control)txtVizierAstorbServer).set_TabIndex(18);
			((Control)txtVizierAstorbServer).set_Text(Settings.Default.ASTORB_Mirror);
			((Control)txtVizierAstorbServer).add_Leave((EventHandler)txtVizierAstorbServer_Leave);
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label9).set_Location(new Point(19, 272));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(48, 13));
			((Control)label9).set_TabIndex(35);
			((Control)label9).set_Text("FutureAll");
			((Control)txtFutureAllFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFutureAllFile).set_Location(new Point(279, 268));
			((Control)txtFutureAllFile).set_Name("txtFutureAllFile");
			((Control)txtFutureAllFile).set_Size(new Size(93, 20));
			((Control)txtFutureAllFile).set_TabIndex(37);
			((Control)txtFutureAllFile).set_Text(Settings.Default.FutureAll_File_XML);
			((Control)txtFutureAllFile).add_Leave((EventHandler)txtFutureAllFile_Leave);
			((Control)txtFutureAllServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFutureAllServer).set_Location(new Point(69, 268));
			((Control)txtFutureAllServer).set_Name("txtFutureAllServer");
			((Control)txtFutureAllServer).set_Size(new Size(204, 20));
			((Control)txtFutureAllServer).set_TabIndex(36);
			((Control)txtFutureAllServer).set_Text(Settings.Default.FutureAll_Server);
			((Control)txtFutureAllServer).add_Leave((EventHandler)txtFutureAllServer_Leave);
			((Control)label8).set_AutoSize(true);
			((Control)label8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label8).set_Location(new Point(30, 251));
			((Control)label8).set_Name("label8");
			((Control)label8).set_Size(new Size(37, 13));
			((Control)label8).set_TabIndex(32);
			((Control)label8).set_Text("Future");
			((Control)txtFutureFile).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "FutureFile_xml", true, (DataSourceUpdateMode)1));
			((Control)txtFutureFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFutureFile).set_Location(new Point(279, 247));
			((Control)txtFutureFile).set_Name("txtFutureFile");
			((Control)txtFutureFile).set_Size(new Size(93, 20));
			((Control)txtFutureFile).set_TabIndex(34);
			((Control)txtFutureFile).set_Text(Settings.Default.FutureFile_xml);
			((Control)txtFutureFile).add_Leave((EventHandler)txtFutureFile_Leave);
			((Control)txtFutureServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtFutureServer).set_Location(new Point(69, 247));
			((Control)txtFutureServer).set_Name("txtFutureServer");
			((Control)txtFutureServer).set_Size(new Size(204, 20));
			((Control)txtFutureServer).set_TabIndex(33);
			((Control)txtFutureServer).set_Text(Settings.Default.Future_Server);
			((Control)txtFutureServer).add_Leave((EventHandler)txtFutureServer_Leave);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(413, 102));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(37, 13));
			((Control)label7).set_TabIndex(53);
			((Control)label7).set_Text("Comet");
			((Control)txtCometFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCometFile).set_Location(new Point(662, 98));
			((Control)txtCometFile).set_Name("txtCometFile");
			((Control)txtCometFile).set_Size(new Size(93, 20));
			((Control)txtCometFile).set_TabIndex(55);
			((Control)txtCometFile).set_Text(Settings.Default.Comet_file);
			((Control)txtCometFile).add_Leave((EventHandler)txtCometFile_Leave);
			((Control)txtCometServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtCometServer).set_Location(new Point(452, 98));
			((Control)txtCometServer).set_Name("txtCometServer");
			((Control)txtCometServer).set_Size(new Size(204, 20));
			((Control)txtCometServer).set_TabIndex(54);
			((Control)txtCometServer).set_Text(Settings.Default.Comet_Server);
			((Control)txtCometServer).add_Leave((EventHandler)txtCometServer_Leave);
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label6).set_Location(new Point(66, 111));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(59, 13));
			((Control)label6).set_TabIndex(20);
			((Control)label6).set_Text("MPCORB");
			((Control)txtMPCOrbFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPCOrbFile).set_Location(new Point(279, 126));
			((Control)txtMPCOrbFile).set_Name("txtMPCOrbFile");
			((Control)txtMPCOrbFile).set_Size(new Size(93, 20));
			((Control)txtMPCOrbFile).set_TabIndex(23);
			((Control)txtMPCOrbFile).set_Text(Settings.Default.MPCOrb_file);
			((Control)txtMPCOrbFile).add_Leave((EventHandler)txtMPCOrbFile_Leave);
			((Control)txtMPCOrbServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtMPCOrbServer).set_Location(new Point(69, 126));
			((Control)txtMPCOrbServer).set_Name("txtMPCOrbServer");
			((Control)txtMPCOrbServer).set_Size(new Size(204, 20));
			((Control)txtMPCOrbServer).set_TabIndex(22);
			((Control)txtMPCOrbServer).set_Text(Settings.Default.MPCOrb_Server);
			((Control)txtMPCOrbServer).add_Leave((EventHandler)txtMPCOrbServer_Leave);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label5).set_Location(new Point(66, 42));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(57, 13));
			((Control)label5).set_TabIndex(13);
			((Control)label5).set_Text("ASTORB");
			((Control)txtAstorbFile).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAstorbFile).set_Location(new Point(279, 57));
			((Control)txtAstorbFile).set_Name("txtAstorbFile");
			((Control)txtAstorbFile).set_Size(new Size(93, 20));
			((Control)txtAstorbFile).set_TabIndex(16);
			((Control)txtAstorbFile).set_Text(Settings.Default.ASTORB_file);
			((Control)txtAstorbFile).add_Leave((EventHandler)txtAstorbFile_Leave);
			((Control)txtAstorbServer).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAstorbServer).set_Location(new Point(69, 57));
			((Control)txtAstorbServer).set_Name("txtAstorbServer");
			((Control)txtAstorbServer).set_Size(new Size(204, 20));
			((Control)txtAstorbServer).set_TabIndex(15);
			((Control)txtAstorbServer).set_Text(Settings.Default.ASTORB_Server);
			((Control)txtAstorbServer).add_Leave((EventHandler)txtAstorbServer_Leave);
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label25).set_Location(new Point(2, 151));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(65, 13));
			((Control)label25).set_TabIndex(24);
			((Control)label25).set_Text("Czech mirror");
			((Control)label128).set_AutoSize(true);
			((Control)label128).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label128).set_Location(new Point(612, 134));
			((Control)label128).set_Name("label128");
			((Control)label128).set_Size(new Size(112, 13));
			((Control)label128).set_TabIndex(9);
			((Control)label128).set_Text("[ '0' = never displayed]");
			label128.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)numericUpDown57).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "DisplayUpdatesFrequency", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown57).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			numericUpDown57.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)numericUpDown57).set_Location(new Point(521, 131));
			((Control)numericUpDown57).set_Name("numericUpDown57");
			((Control)numericUpDown57).set_Size(new Size(38, 20));
			((Control)numericUpDown57).set_TabIndex(7);
			numericUpDown57.set_Value(Settings.Default.DisplayUpdatesFrequency);
			((Control)label127).set_AutoSize(true);
			((Control)label127).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label127).set_Location(new Point(560, 134));
			((Control)label127).set_Name("label127");
			((Control)label127).set_Size(new Size(31, 13));
			((Control)label127).set_TabIndex(8);
			((Control)label127).set_Text("Days");
			label127.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label126).set_AutoSize(true);
			((Control)label126).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label126).set_Location(new Point(16, 134));
			((Control)label126).set_Name("label126");
			((Control)label126).set_Size(new Size(503, 13));
			((Control)label126).set_TabIndex(6);
			((Control)label126).set_Text("At start-up, Occult will display a page for downloading updated data if that page hasn't been displayed for ");
			label126.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)cmdHelpInternet);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)label125);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)txtPort);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)checkBox5);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)label124);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)label123);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)label122);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)label121);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)textBox6);
			((Control)grpEmailAdvanced).get_Controls().Add((Control)(object)textBox5);
			((Control)grpEmailAdvanced).set_Font(new Font("Microsoft Sans Serif", 9f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpEmailAdvanced).set_Location(new Point(16, 66));
			((Control)grpEmailAdvanced).set_Name("grpEmailAdvanced");
			((Control)grpEmailAdvanced).set_Size(new Size(733, 61));
			((Control)grpEmailAdvanced).set_TabIndex(5);
			grpEmailAdvanced.set_TabStop(false);
			((Control)grpEmailAdvanced).set_Text("Advanced Email settings - specify only if needed");
			((Control)cmdHelpInternet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdHelpInternet).set_Location(new Point(12, 23));
			((Control)cmdHelpInternet).set_Name("cmdHelpInternet");
			((Control)cmdHelpInternet).set_Size(new Size(46, 29));
			((Control)cmdHelpInternet).set_TabIndex(0);
			((Control)cmdHelpInternet).set_Text("Help");
			((ButtonBase)cmdHelpInternet).set_UseVisualStyleBackColor(true);
			((Control)cmdHelpInternet).add_Click((EventHandler)cmdHelpInternet_Click);
			((Control)label125).set_AutoSize(true);
			((Control)label125).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label125).set_Location(new Point(555, 26));
			((Control)label125).set_Name("label125");
			((Control)label125).set_Size(new Size(124, 26));
			((Control)label125).set_TabIndex(7);
			((Control)label125).set_Text("If your Email server uses \r\na Port other than 25");
			label125.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)txtPort).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EmailPort", true, (DataSourceUpdateMode)1));
			((Control)txtPort).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtPort).set_Location(new Point(681, 32));
			((Control)txtPort).set_Name("txtPort");
			((Control)txtPort).set_Size(new Size(40, 20));
			((Control)txtPort).set_TabIndex(9);
			((Control)txtPort).set_Text(Settings.Default.EmailPort);
			((Control)checkBox5).set_AutoSize(true);
			checkBox5.set_Checked(Settings.Default.EmailUseSSL);
			((Control)checkBox5).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "EmailUseSSL", true, (DataSourceUpdateMode)1));
			((Control)checkBox5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox5).set_Location(new Point(426, 24));
			((Control)checkBox5).set_Name("checkBox5");
			((Control)checkBox5).set_Size(new Size(123, 30));
			((Control)checkBox5).set_TabIndex(6);
			((Control)checkBox5).set_Text("Email server requires\r\nSSL connection");
			((ButtonBase)checkBox5).set_UseVisualStyleBackColor(true);
			((Control)label124).set_AutoSize(true);
			((Control)label124).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label124).set_Location(new Point(684, 14));
			((Control)label124).set_Name("label124");
			((Control)label124).set_Size(new Size(26, 13));
			((Control)label124).set_TabIndex(8);
			((Control)label124).set_Text("Port");
			label124.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label123).set_AutoSize(true);
			((Control)label123).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label123).set_Location(new Point(238, 18));
			((Control)label123).set_Name("label123");
			((Control)label123).set_Size(new Size(84, 13));
			((Control)label123).set_TabIndex(2);
			((Control)label123).set_Text("Email user name");
			label123.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label122).set_AutoSize(true);
			((Control)label122).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label122).set_Location(new Point(356, 18));
			((Control)label122).set_Name("label122");
			((Control)label122).set_Size(new Size(53, 13));
			((Control)label122).set_TabIndex(4);
			((Control)label122).set_Text("Password");
			label122.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label121).set_AutoSize(true);
			((Control)label121).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label121).set_Location(new Point(71, 26));
			((Control)label121).set_Name("label121");
			((Control)label121).set_Size(new Size(139, 26));
			((Control)label121).set_TabIndex(1);
			((Control)label121).set_Text("If your Email server requires \r\nseparate authentication");
			label121.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)textBox6).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "Email", true, (DataSourceUpdateMode)1));
			((Control)textBox6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox6).set_Location(new Point(354, 32));
			((Control)textBox6).set_Name("textBox6");
			textBox6.set_PasswordChar('*');
			((Control)textBox6).set_Size(new Size(55, 20));
			((Control)textBox6).set_TabIndex(5);
			((Control)textBox6).set_Text(Settings.Default.Email);
			((Control)textBox5).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EmailUser", true, (DataSourceUpdateMode)1));
			((Control)textBox5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox5).set_Location(new Point(213, 32));
			((Control)textBox5).set_Name("textBox5");
			((Control)textBox5).set_Size(new Size(132, 20));
			((Control)textBox5).set_TabIndex(3);
			((Control)textBox5).set_Text(Settings.Default.EmailUser);
			((Control)label119).set_AutoSize(true);
			((Control)label119).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label119).set_Location(new Point(16, 23));
			((Control)label119).set_Name("label119");
			((Control)label119).set_Size(new Size(497, 13));
			((Control)label119).set_TabIndex(0);
			((Control)label119).set_Text("To  Email your observations, you must enter your email address and your SMTP email server name here. ");
			label119.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)textBox4).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "EMailServerName", true, (DataSourceUpdateMode)1));
			((Control)textBox4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox4).set_Location(new Point(508, 40));
			((Control)textBox4).set_Name("textBox4");
			((Control)textBox4).set_Size(new Size(221, 20));
			((Control)textBox4).set_TabIndex(4);
			((Control)textBox4).set_Text(Settings.Default.EMailServerName);
			((Control)label118).set_AutoSize(true);
			((Control)label118).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label118).set_Location(new Point(376, 43));
			((Control)label118).set_Name("label118");
			((Control)label118).set_Size(new Size(130, 13));
			((Control)label118).set_TabIndex(3);
			((Control)label118).set_Text("SMTP Email Server Name");
			label118.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label74).set_AutoSize(true);
			((Control)label74).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label74).set_Location(new Point(14, 163));
			((Control)label74).set_Name("label74");
			((Control)label74).set_Size(new Size(716, 39));
			((Control)label74).set_TabIndex(10);
			((Control)label74).set_Text(componentResourceManager.GetString("label74.Text"));
			((Control)label10).set_AutoSize(true);
			((Control)label10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label10).set_Location(new Point(15, 43));
			((Control)label10).set_Name("label10");
			((Control)label10).set_Size(new Size(103, 13));
			((Control)label10).set_TabIndex(1);
			((Control)label10).set_Text("User's email address");
			label10.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)FTPPassword).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "FTP_AnonymousPassword", true, (DataSourceUpdateMode)1));
			((Control)FTPPassword).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)FTPPassword).set_Location(new Point(118, 40));
			((Control)FTPPassword).set_Name("FTPPassword");
			((Control)FTPPassword).set_Size(new Size(217, 20));
			((Control)FTPPassword).set_TabIndex(2);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label12).set_Location(new Point(9, 49));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(210, 13));
			((Control)label12).set_TabIndex(1);
			((Control)label12).set_Text("Uncertainty of position of asteroid (arcsecs)");
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(9, 139));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(20, 13));
			((Control)label20).set_TabIndex(21);
			((Control)label20).set_Text("#4");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(9, 116));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(20, 13));
			((Control)label19).set_TabIndex(16);
			((Control)label19).set_Text("#3");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(9, 93));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(20, 13));
			((Control)label18).set_TabIndex(11);
			((Control)label18).set_Text("#2");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(9, 70));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(20, 13));
			((Control)label17).set_TabIndex(5);
			((Control)label17).set_Text("#1");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label13).set_Location(new Point(223, 51));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(35, 13));
			((Control)label13).set_TabIndex(4);
			((Control)label13).set_Text("South");
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label14).set_Location(new Point(168, 51));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(33, 13));
			((Control)label14).set_TabIndex(3);
			((Control)label14).set_Text("North");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label15).set_Location(new Point(100, 51));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(28, 13));
			((Control)label15).set_TabIndex(2);
			((Control)label15).set_Text("East");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(32, 51));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(32, 13));
			((Control)label16).set_TabIndex(1);
			((Control)label16).set_Text("West");
			((Control)grpStarChartInitialise).set_BackColor(Color.AntiqueWhite);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)checkBox7);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkGaia16Auto);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label175);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)checkBox8);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)checkBox4);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkGaia);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkGaia12);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label117);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label163);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkPPMXL);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkAutoPPMXL);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkAutoUCAC4);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkUCAC4);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkAutoNomad);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)cmbAutoSizeDeg);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)cmbAutoMagLimit);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)cmbAutoSizePixels);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label112);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label110);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label111);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label109);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label26);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)chkNomad);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label22);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)label21);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)cmbMag);
			((Control)grpStarChartInitialise).get_Controls().Add((Control)(object)cmbSize);
			((Control)grpStarChartInitialise).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpStarChartInitialise).set_Location(new Point(13, 2003));
			((Control)grpStarChartInitialise).set_Name("grpStarChartInitialise");
			((Control)grpStarChartInitialise).set_Size(new Size(758, 157));
			((Control)grpStarChartInitialise).set_TabIndex(7);
			grpStarChartInitialise.set_TabStop(false);
			((Control)grpStarChartInitialise).set_Text("9. Star Chart start-up, and AutoGenerate");
			((Control)checkBox7).set_AutoSize(true);
			checkBox7.set_CheckAlign(ContentAlignment.MiddleRight);
			checkBox7.set_Checked(Settings.Default.StarChartUseGaia16);
			((Control)checkBox7).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "StarChartUseGaia16", true, (DataSourceUpdateMode)1));
			((Control)checkBox7).set_Enabled(false);
			((Control)checkBox7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox7).set_Location(new Point(585, 64));
			((Control)checkBox7).set_Name("checkBox7");
			((Control)checkBox7).set_Size(new Size(60, 17));
			((Control)checkBox7).set_TabIndex(31);
			((Control)checkBox7).set_Text("Gaia16");
			((ButtonBase)checkBox7).set_UseVisualStyleBackColor(true);
			((Control)checkBox7).set_Visible(false);
			((Control)chkGaia16Auto).set_AutoSize(true);
			chkGaia16Auto.set_CheckAlign(ContentAlignment.MiddleRight);
			chkGaia16Auto.set_Checked(Settings.Default.AutoGenerateUseGaia16);
			((Control)chkGaia16Auto).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoGenerateUseGaia16", true, (DataSourceUpdateMode)1));
			((Control)chkGaia16Auto).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkGaia16Auto).set_Location(new Point(585, 129));
			((Control)chkGaia16Auto).set_Name("chkGaia16Auto");
			((Control)chkGaia16Auto).set_Size(new Size(60, 17));
			((Control)chkGaia16Auto).set_TabIndex(30);
			((Control)chkGaia16Auto).set_Text("Gaia16");
			((ButtonBase)chkGaia16Auto).set_UseVisualStyleBackColor(true);
			((Control)label175).set_AutoSize(true);
			((Control)label175).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label175).set_Location(new Point(447, 121));
			((Control)label175).set_Name("label175");
			((Control)label175).set_Size(new Size(29, 13));
			((Control)label175).set_TabIndex(29);
			((Control)label175).set_Text("Use:");
			((Control)checkBox8).set_AutoSize(true);
			checkBox8.set_CheckAlign(ContentAlignment.MiddleRight);
			checkBox8.set_Checked(Settings.Default.AutoGenerateUseGaia14);
			((Control)checkBox8).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoGenerateUseGaia14", true, (DataSourceUpdateMode)1));
			((Control)checkBox8).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox8).set_Location(new Point(493, 129));
			((Control)checkBox8).set_Name("checkBox8");
			((Control)checkBox8).set_Size(new Size(60, 17));
			((Control)checkBox8).set_TabIndex(28);
			((Control)checkBox8).set_Text("Gaia14");
			((ButtonBase)checkBox8).set_UseVisualStyleBackColor(true);
			((Control)checkBox4).set_AutoSize(true);
			checkBox4.set_CheckAlign(ContentAlignment.MiddleRight);
			checkBox4.set_Checked(Settings.Default.AutoGenerateUseTychoGaia);
			((Control)checkBox4).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoGenerateUseTychoGaia", true, (DataSourceUpdateMode)1));
			((Control)checkBox4).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox4).set_Location(new Point(493, 112));
			((Control)checkBox4).set_Name("checkBox4");
			((Control)checkBox4).set_Size(new Size(60, 17));
			((Control)checkBox4).set_TabIndex(27);
			((Control)checkBox4).set_Text("Gaia12");
			((ButtonBase)checkBox4).set_UseVisualStyleBackColor(true);
			((Control)chkGaia).set_AutoSize(true);
			chkGaia.set_CheckAlign(ContentAlignment.MiddleRight);
			chkGaia.set_Checked(Settings.Default.StarChartUseGaia14);
			((Control)chkGaia).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "StarChartUseGaia14", true, (DataSourceUpdateMode)1));
			((Control)chkGaia).set_Enabled(false);
			((Control)chkGaia).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkGaia).set_Location(new Point(494, 64));
			((Control)chkGaia).set_Name("chkGaia");
			((Control)chkGaia).set_Size(new Size(60, 17));
			((Control)chkGaia).set_TabIndex(26);
			((Control)chkGaia).set_Text("Gaia14");
			((ButtonBase)chkGaia).set_UseVisualStyleBackColor(true);
			((Control)chkGaia).set_Visible(false);
			((Control)chkGaia12).set_AutoSize(true);
			chkGaia12.set_CheckAlign(ContentAlignment.MiddleRight);
			chkGaia12.set_Checked(Settings.Default.StarChartUseGaia);
			chkGaia12.set_CheckState((CheckState)1);
			((Control)chkGaia12).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "StarChartUseGaia", true, (DataSourceUpdateMode)1));
			((Control)chkGaia12).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkGaia12).set_Location(new Point(506, 41));
			((Control)chkGaia12).set_Name("chkGaia12");
			((Control)chkGaia12).set_Size(new Size(48, 17));
			((Control)chkGaia12).set_TabIndex(25);
			((Control)chkGaia12).set_Text("Gaia");
			((ButtonBase)chkGaia12).set_UseVisualStyleBackColor(true);
			((Control)label117).set_AutoSize(true);
			((Control)label117).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label117).set_Location(new Point(395, 47));
			((Control)label117).set_Name("label117");
			((Control)label117).set_Size(new Size(76, 13));
			((Control)label117).set_TabIndex(24);
			((Control)label117).set_Text("Default is Gaia");
			((Control)label163).set_AutoSize(true);
			((Control)label163).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label163).set_Location(new Point(390, 60));
			((Control)label163).set_Name("label163");
			((Control)label163).set_Size(new Size(86, 13));
			((Control)label163).set_TabIndex(21);
			((Control)label163).set_Text("(check only one)");
			((Control)chkPPMXL).set_AutoSize(true);
			chkPPMXL.set_CheckAlign(ContentAlignment.MiddleRight);
			chkPPMXL.set_Checked(Settings.Default.StarChartUsePPMXL);
			((Control)chkPPMXL).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "StarChartUsePPMXL", true, (DataSourceUpdateMode)1));
			((Control)chkPPMXL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPPMXL).set_Location(new Point(683, 41));
			((Control)chkPPMXL).set_Name("chkPPMXL");
			((Control)chkPPMXL).set_Size(new Size(62, 17));
			((Control)chkPPMXL).set_TabIndex(20);
			((Control)chkPPMXL).set_Text("PPMXL");
			((ButtonBase)chkPPMXL).set_UseVisualStyleBackColor(true);
			((Control)chkAutoPPMXL).set_AutoSize(true);
			chkAutoPPMXL.set_CheckAlign(ContentAlignment.MiddleRight);
			chkAutoPPMXL.set_Checked(Settings.Default.AutoGenerateUsePPMXL);
			((Control)chkAutoPPMXL).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoGenerateUsePPMXL", true, (DataSourceUpdateMode)1));
			((Control)chkAutoPPMXL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAutoPPMXL).set_Location(new Point(683, 112));
			((Control)chkAutoPPMXL).set_Name("chkAutoPPMXL");
			((Control)chkAutoPPMXL).set_Size(new Size(62, 17));
			((Control)chkAutoPPMXL).set_TabIndex(19);
			((Control)chkAutoPPMXL).set_Text("PPMXL");
			((ButtonBase)chkAutoPPMXL).set_UseVisualStyleBackColor(true);
			((Control)chkAutoUCAC4).set_AutoSize(true);
			chkAutoUCAC4.set_CheckAlign(ContentAlignment.MiddleRight);
			chkAutoUCAC4.set_Checked(Settings.Default.AutoGenerateUseUCAC4);
			((Control)chkAutoUCAC4).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoGenerateUseUCAC4", true, (DataSourceUpdateMode)1));
			((Control)chkAutoUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAutoUCAC4).set_Location(new Point(584, 112));
			((Control)chkAutoUCAC4).set_Name("chkAutoUCAC4");
			((Control)chkAutoUCAC4).set_Size(new Size(61, 17));
			((Control)chkAutoUCAC4).set_TabIndex(18);
			((Control)chkAutoUCAC4).set_Text("UCAC4");
			((ButtonBase)chkAutoUCAC4).set_UseVisualStyleBackColor(true);
			((Control)chkUCAC4).set_AutoSize(true);
			chkUCAC4.set_CheckAlign(ContentAlignment.MiddleRight);
			chkUCAC4.set_Checked(Settings.Default.StarChartUseUCAC4);
			((Control)chkUCAC4).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "StarChartUseUCAC4", true, (DataSourceUpdateMode)1));
			((Control)chkUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkUCAC4).set_Location(new Point(584, 41));
			((Control)chkUCAC4).set_Name("chkUCAC4");
			((Control)chkUCAC4).set_Size(new Size(61, 17));
			((Control)chkUCAC4).set_TabIndex(17);
			((Control)chkUCAC4).set_Text("UCAC4");
			((ButtonBase)chkUCAC4).set_UseVisualStyleBackColor(true);
			((Control)chkAutoNomad).set_AutoSize(true);
			chkAutoNomad.set_CheckAlign(ContentAlignment.MiddleRight);
			chkAutoNomad.set_Checked(Settings.Default.AutoGenerateUseNomad);
			((Control)chkAutoNomad).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoGenerateUseNomad", true, (DataSourceUpdateMode)1));
			((Control)chkAutoNomad).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAutoNomad).set_Location(new Point(685, 129));
			((Control)chkAutoNomad).set_Name("chkAutoNomad");
			((Control)chkAutoNomad).set_Size(new Size(60, 17));
			((Control)chkAutoNomad).set_TabIndex(16);
			((Control)chkAutoNomad).set_Text("Nomad");
			((ButtonBase)chkAutoNomad).set_UseVisualStyleBackColor(true);
			cmbAutoSizeDeg.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAutoSizeDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAutoSizeDeg).set_FormattingEnabled(true);
			cmbAutoSizeDeg.get_Items().AddRange(new object[14]
			{
				"0.2 x 0.2 deg", "0.4 x 0.4 deg", "0.7 x 0.7 deg", "1 x 1 deg", "2 x 2 deg", "3 x 3 deg", "4 x 4 deg", "5 x 5 deg", "6 x 6 deg", "8 x 8 deg",
				"10 x 10 deg", "12 x 12 deg", "15 x 15 deg", "20 x 20 deg"
			});
			((Control)cmbAutoSizeDeg).set_Location(new Point(224, 117));
			cmbAutoSizeDeg.set_MaxDropDownItems(14);
			((Control)cmbAutoSizeDeg).set_Name("cmbAutoSizeDeg");
			((Control)cmbAutoSizeDeg).set_Size(new Size(90, 21));
			((Control)cmbAutoSizeDeg).set_TabIndex(11);
			cmbAutoSizeDeg.add_SelectedIndexChanged((EventHandler)cmbAutoSizeDeg_SelectedIndexChanged);
			cmbAutoMagLimit.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAutoMagLimit).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAutoMagLimit).set_FormattingEnabled(true);
			cmbAutoMagLimit.get_Items().AddRange(new object[15]
			{
				"<20.0", "<19.0", "<18.0", "<17.0", "<16.0", "<15.0", "<14.0", "<13.0", "<12.0", "<11.0",
				"<10.0", "<9.0", "<8.0", "<7.0", "<6.0"
			});
			((Control)cmbAutoMagLimit).set_Location(new Point(376, 117));
			cmbAutoMagLimit.set_MaxDropDownItems(14);
			((Control)cmbAutoMagLimit).set_Name("cmbAutoMagLimit");
			((Control)cmbAutoMagLimit).set_Size(new Size(60, 21));
			((Control)cmbAutoMagLimit).set_TabIndex(13);
			cmbAutoMagLimit.add_SelectedIndexChanged((EventHandler)cmbAutoMagLimit_SelectedIndexChanged);
			cmbAutoSizePixels.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAutoSizePixels).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAutoSizePixels).set_FormattingEnabled(true);
			cmbAutoSizePixels.get_Items().AddRange(new object[3] { "480 x 480", "768 x 768", "1024 x 1024" });
			((Control)cmbAutoSizePixels).set_Location(new Point(72, 117));
			((Control)cmbAutoSizePixels).set_Name("cmbAutoSizePixels");
			((Control)cmbAutoSizePixels).set_Size(new Size(90, 21));
			((Control)cmbAutoSizePixels).set_TabIndex(9);
			cmbAutoSizePixels.add_SelectedIndexChanged((EventHandler)cmbAutoSizePixels_SelectedIndexChanged);
			((Control)label112).set_AutoSize(true);
			((Control)label112).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label112).set_Location(new Point(9, 121));
			((Control)label112).set_Name("label112");
			((Control)label112).set_Size(new Size(62, 13));
			((Control)label112).set_TabIndex(8);
			((Control)label112).set_Text("Size (pixels)");
			((Control)label110).set_AutoSize(true);
			((Control)label110).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label110).set_Location(new Point(325, 121));
			((Control)label110).set_Name("label110");
			((Control)label110).set_Size(new Size(51, 13));
			((Control)label110).set_TabIndex(12);
			((Control)label110).set_Text("Mag. limit");
			((Control)label111).set_AutoSize(true);
			((Control)label111).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label111).set_Location(new Point(168, 121));
			((Control)label111).set_Name("label111");
			((Control)label111).set_Size(new Size(54, 13));
			((Control)label111).set_TabIndex(10);
			((Control)label111).set_Text("Size (deg)");
			((Control)label109).set_AutoSize(true);
			((Control)label109).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label109).set_Location(new Point(12, 96));
			((Control)label109).set_Name("label109");
			((Control)label109).set_Size(new Size(608, 13));
			((Control)label109).set_TabIndex(7);
			((Control)label109).set_Text("Occult can also Auto-generate a star chart for asteroid occultation predictions. Specify the AutoGenerate chart parameters here.");
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label26).set_Location(new Point(10, 23));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(588, 13));
			((Control)label26).set_TabIndex(0);
			((Control)label26).set_Text("Occult can generates a star chart for any part of the sky, and for asteroid occultations. Specify the start-up parameters here.");
			((Control)chkNomad).set_AutoSize(true);
			chkNomad.set_CheckAlign(ContentAlignment.MiddleRight);
			chkNomad.set_Checked(Settings.Default.StarChartUseNomad);
			((Control)chkNomad).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "StarChartUseNomad", true, (DataSourceUpdateMode)1));
			((Control)chkNomad).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkNomad).set_Location(new Point(685, 64));
			((Control)chkNomad).set_Name("chkNomad");
			((Control)chkNomad).set_Size(new Size(60, 17));
			((Control)chkNomad).set_TabIndex(6);
			((Control)chkNomad).set_Text("Nomad");
			((ButtonBase)chkNomad).set_UseVisualStyleBackColor(true);
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label22).set_Location(new Point(196, 51));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(77, 13));
			((Control)label22).set_TabIndex(2);
			((Control)label22).set_Text("Magnitude limit");
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(22, 51));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(54, 13));
			((Control)label21).set_TabIndex(0);
			((Control)label21).set_Text("Size (deg)");
			cmbMag.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMag).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMag).set_FormattingEnabled(true);
			cmbMag.get_Items().AddRange(new object[17]
			{
				"6.0", "7.0", "8.0", "9.0", "10.0", "11.0", "12.0", "13.0", "14.0", "15.0",
				"16.0", "17.0", "18.0", "19.0", "20.0", "21.0", "All"
			});
			((Control)cmbMag).set_Location(new Point(275, 47));
			cmbMag.set_MaxDropDownItems(16);
			((Control)cmbMag).set_Name("cmbMag");
			((Control)cmbMag).set_Size(new Size(66, 21));
			((Control)cmbMag).set_TabIndex(3);
			cmbMag.add_SelectedIndexChanged((EventHandler)cmbMag_SelectedIndexChanged);
			cmbSize.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbSize).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSize).set_FormattingEnabled(true);
			cmbSize.get_Items().AddRange(new object[19]
			{
				"10' x 10'", "12' x 12'", "15' x 15'", "18' x 18'", "24' x 24'", "30' x 30'", "40' x 40'", "50' x 50'", "1° x 1°", "2° x 2°",
				"3° x 3°", "4° x 4°", "5° x 5°", "6° x 6°", "8° x 8°", "10° x 10°", "12° x 12°", "15° x 15°", "20° x 20°"
			});
			((Control)cmbSize).set_Location(new Point(79, 47));
			cmbSize.set_MaxDropDownItems(15);
			((Control)cmbSize).set_Name("cmbSize");
			((Control)cmbSize).set_Size(new Size(90, 21));
			((Control)cmbSize).set_TabIndex(1);
			cmbSize.add_SelectedIndexChanged((EventHandler)cmbSize_SelectedIndexChanged);
			((ToolStripItem)x1DegToolStripMenuItem).set_Name("x1DegToolStripMenuItem");
			((ToolStripItem)x1DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x1DegToolStripMenuItem).set_Text("1 x 1 deg");
			((ToolStripItem)x2DegToolStripMenuItem).set_Name("x2DegToolStripMenuItem");
			((ToolStripItem)x2DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x2DegToolStripMenuItem).set_Text("2 x 2 deg");
			((ToolStripItem)x3DegToolStripMenuItem).set_Name("x3DegToolStripMenuItem");
			((ToolStripItem)x3DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x3DegToolStripMenuItem).set_Text("3 x 3 deg");
			((ToolStripItem)toolStripMenuItem11).set_Name("toolStripMenuItem11");
			((ToolStripItem)toolStripMenuItem11).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem11).set_Text("4 x 4 deg");
			((ToolStripItem)x5DegToolStripMenuItem).set_Name("x5DegToolStripMenuItem");
			((ToolStripItem)x5DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x5DegToolStripMenuItem).set_Text("5 x 5 deg");
			((ToolStripItem)x6DegToolStripMenuItem).set_Name("x6DegToolStripMenuItem");
			((ToolStripItem)x6DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x6DegToolStripMenuItem).set_Text("6 x 6 deg");
			((ToolStripItem)x8DegToolStripMenuItem).set_Name("x8DegToolStripMenuItem");
			((ToolStripItem)x8DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x8DegToolStripMenuItem).set_Text("8 x 8 deg");
			((ToolStripItem)x10DegToolStripMenuItem).set_Name("x10DegToolStripMenuItem");
			((ToolStripItem)x10DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x10DegToolStripMenuItem).set_Text("10 x 10 deg");
			((ToolStripItem)x12DegToolStripMenuItem).set_Name("x12DegToolStripMenuItem");
			((ToolStripItem)x12DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x12DegToolStripMenuItem).set_Text("12 x 12 deg");
			((ToolStripItem)x15DegToolStripMenuItem).set_Name("x15DegToolStripMenuItem");
			((ToolStripItem)x15DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x15DegToolStripMenuItem).set_Text("15 x 15 deg");
			((ToolStripItem)x20DegToolStripMenuItem).set_Name("x20DegToolStripMenuItem");
			((ToolStripItem)x20DegToolStripMenuItem).set_Size(new Size(142, 22));
			((ToolStripItem)x20DegToolStripMenuItem).set_Text("20 x 20 deg");
			((ToolStripItem)toolStripMenuItem1).set_Name("toolStripMenuItem1");
			((ToolStripItem)toolStripMenuItem1).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem1).set_Text("1 x 1 deg");
			((ToolStripItem)toolStripMenuItem2).set_Name("toolStripMenuItem2");
			((ToolStripItem)toolStripMenuItem2).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem2).set_Text("2 x 2 deg");
			((ToolStripItem)toolStripMenuItem3).set_Name("toolStripMenuItem3");
			((ToolStripItem)toolStripMenuItem3).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem3).set_Text("3 x 3 deg");
			((ToolStripItem)toolStripMenuItem4).set_Name("toolStripMenuItem4");
			((ToolStripItem)toolStripMenuItem4).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem4).set_Text("4 x 4 deg");
			((ToolStripItem)toolStripMenuItem5).set_Name("toolStripMenuItem5");
			((ToolStripItem)toolStripMenuItem5).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem5).set_Text("5 x 5 deg");
			((ToolStripItem)toolStripMenuItem6).set_Name("toolStripMenuItem6");
			((ToolStripItem)toolStripMenuItem6).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem6).set_Text("6 x 6 deg");
			((ToolStripItem)toolStripMenuItem7).set_Name("toolStripMenuItem7");
			((ToolStripItem)toolStripMenuItem7).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem7).set_Text("8 x 8 deg");
			((ToolStripItem)toolStripMenuItem8).set_Name("toolStripMenuItem8");
			((ToolStripItem)toolStripMenuItem8).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem8).set_Text("10 x 10 deg");
			((ToolStripItem)toolStripMenuItem9).set_Name("toolStripMenuItem9");
			((ToolStripItem)toolStripMenuItem9).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem9).set_Text("12 x 12 deg");
			((ToolStripItem)toolStripMenuItem10).set_Name("toolStripMenuItem10");
			((ToolStripItem)toolStripMenuItem10).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem10).set_Text("15 x 15 deg");
			((ToolStripItem)toolStripMenuItem12).set_Name("toolStripMenuItem12");
			((ToolStripItem)toolStripMenuItem12).set_Size(new Size(142, 22));
			((ToolStripItem)toolStripMenuItem12).set_Text("20 x 20 deg");
			((ScrollableControl)panel1).set_AutoScroll(true);
			((ScrollableControl)panel1).set_AutoScrollMargin(new Size(0, 10));
			((ScrollableControl)panel1).set_AutoScrollMinSize(new Size(600, 500));
			panel1.set_AutoSizeMode((AutoSizeMode)0);
			panel1.set_BorderStyle((BorderStyle)1);
			((Control)panel1).get_Controls().Add((Control)(object)grpC2A);
			((Control)panel1).get_Controls().Add((Control)(object)grpURLs);
			((Control)panel1).get_Controls().Add((Control)(object)grpEmails);
			((Control)panel1).get_Controls().Add((Control)(object)grpTest);
			((Control)panel1).get_Controls().Add((Control)(object)grpUpdates);
			((Control)panel1).get_Controls().Add((Control)(object)grpAsteroidMultiPathMaps);
			((Control)panel1).get_Controls().Add((Control)(object)grpLunarReports);
			((Control)panel1).get_Controls().Add((Control)(object)grpAdministrator);
			((Control)panel1).get_Controls().Add((Control)(object)grpSiteFiles);
			((Control)panel1).get_Controls().Add((Control)(object)grpFileSaveGraphics);
			((Control)panel1).get_Controls().Add((Control)(object)grpAsteroidSearchDisplay);
			((Control)panel1).get_Controls().Add((Control)(object)grpGoogleEarth);
			((Control)panel1).get_Controls().Add((Control)(object)grpOptionalCatalogues);
			((Control)panel1).get_Controls().Add((Control)(object)grpLunarOccultations);
			((Control)panel1).get_Controls().Add((Control)(object)grpStarChartInitialise);
			((Control)panel1).get_Controls().Add((Control)(object)grpHomeLocation);
			((Control)panel1).set_Location(new Point(1, 27));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(798, 560));
			((Control)panel1).set_TabIndex(1);
			((Control)grpC2A).set_BackColor(Color.AntiqueWhite);
			((Control)grpC2A).get_Controls().Add((Control)(object)pictureBox3);
			((Control)grpC2A).get_Controls().Add((Control)(object)chkC2A_MultipleInstances);
			((Control)grpC2A).get_Controls().Add((Control)(object)label179);
			((Control)grpC2A).get_Controls().Add((Control)(object)optUCAC4);
			((Control)grpC2A).get_Controls().Add((Control)(object)optPPMXL);
			((Control)grpC2A).get_Controls().Add((Control)(object)optNOMAD);
			((Control)grpC2A).get_Controls().Add((Control)(object)optSAO);
			((Control)grpC2A).get_Controls().Add((Control)(object)cmbC2AFieldSize);
			((Control)grpC2A).get_Controls().Add((Control)(object)label146);
			((Control)grpC2A).get_Controls().Add((Control)(object)label178);
			((Control)grpC2A).get_Controls().Add((Control)(object)cmdFolder_C2A);
			((Control)grpC2A).get_Controls().Add((Control)(object)txtC2A);
			((Control)grpC2A).get_Controls().Add((Control)(object)label177);
			((Control)grpC2A).get_Controls().Add((Control)(object)linkC2A);
			((Control)grpC2A).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpC2A).set_Location(new Point(12, 2192));
			((Control)grpC2A).set_Name("grpC2A");
			((Control)grpC2A).set_Size(new Size(758, 109));
			((Control)grpC2A).set_TabIndex(16);
			grpC2A.set_TabStop(false);
			((Control)grpC2A).set_Text("10. C2A  Planetarium software");
			pictureBox3.set_Image((Image)Resources.c2aw_0000);
			((Control)pictureBox3).set_Location(new Point(7, 19));
			((Control)pictureBox3).set_Name("pictureBox3");
			((Control)pictureBox3).set_Size(new Size(35, 32));
			pictureBox3.set_TabIndex(15);
			pictureBox3.set_TabStop(false);
			((Control)chkC2A_MultipleInstances).set_AutoSize(true);
			chkC2A_MultipleInstances.set_CheckAlign(ContentAlignment.BottomCenter);
			chkC2A_MultipleInstances.set_Checked(Settings.Default.C2APreventMultipleInstances);
			((Control)chkC2A_MultipleInstances).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "C2APreventMultipleInstances", true, (DataSourceUpdateMode)1));
			((Control)chkC2A_MultipleInstances).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkC2A_MultipleInstances).set_Location(new Point(400, 46));
			((Control)chkC2A_MultipleInstances).set_Name("chkC2A_MultipleInstances");
			((Control)chkC2A_MultipleInstances).set_Size(new Size(91, 44));
			((Control)chkC2A_MultipleInstances).set_TabIndex(5);
			((Control)chkC2A_MultipleInstances).set_Text("Prevent multiple \r\ninstances of C2A");
			((ButtonBase)chkC2A_MultipleInstances).set_UseVisualStyleBackColor(true);
			((Control)label179).set_AutoSize(true);
			((Control)label179).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label179).set_Location(new Point(616, 50));
			((Control)label179).set_Name("label179");
			((Control)label179).set_Size(new Size(130, 13));
			((Control)label179).set_TabIndex(8);
			((Control)label179).set_Text("Star catalog to use in C2A");
			((Control)optUCAC4).set_AutoSize(true);
			((Control)optUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optUCAC4).set_Location(new Point(624, 82));
			((Control)optUCAC4).set_Name("optUCAC4");
			((Control)optUCAC4).set_Size(new Size(60, 17));
			((Control)optUCAC4).set_TabIndex(10);
			optUCAC4.set_TabStop(true);
			((Control)optUCAC4).set_Text("UCAC4");
			((ButtonBase)optUCAC4).set_UseVisualStyleBackColor(true);
			((Control)optUCAC4).add_Click((EventHandler)optUCAC4_Click);
			((Control)optPPMXL).set_AutoSize(true);
			((Control)optPPMXL).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optPPMXL).set_Location(new Point(685, 66));
			((Control)optPPMXL).set_Name("optPPMXL");
			((Control)optPPMXL).set_Size(new Size(61, 17));
			((Control)optPPMXL).set_TabIndex(11);
			optPPMXL.set_TabStop(true);
			((Control)optPPMXL).set_Text("PPMXL");
			((ButtonBase)optPPMXL).set_UseVisualStyleBackColor(true);
			((Control)optPPMXL).add_Click((EventHandler)optPPMXL_Click);
			((Control)optNOMAD).set_AutoSize(true);
			((Control)optNOMAD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optNOMAD).set_Location(new Point(685, 82));
			((Control)optNOMAD).set_Name("optNOMAD");
			((Control)optNOMAD).set_Size(new Size(65, 17));
			((Control)optNOMAD).set_TabIndex(12);
			optNOMAD.set_TabStop(true);
			((Control)optNOMAD).set_Text("NOMAD");
			((ButtonBase)optNOMAD).set_UseVisualStyleBackColor(true);
			((Control)optNOMAD).add_Click((EventHandler)optNOMAD_Click);
			((Control)optSAO).set_AutoSize(true);
			((Control)optSAO).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optSAO).set_Location(new Point(624, 66));
			((Control)optSAO).set_Name("optSAO");
			((Control)optSAO).set_Size(new Size(47, 17));
			((Control)optSAO).set_TabIndex(9);
			optSAO.set_TabStop(true);
			((Control)optSAO).set_Text("SAO");
			((ButtonBase)optSAO).set_UseVisualStyleBackColor(true);
			((Control)optSAO).add_Click((EventHandler)optSAO_Click);
			((Control)cmbC2AFieldSize).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "C2AFieldSize", true, (DataSourceUpdateMode)1));
			((Control)cmbC2AFieldSize).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbC2AFieldSize).set_FormattingEnabled(true);
			cmbC2AFieldSize.get_Items().AddRange(new object[9] { "10°", " 5°", " 2°", " 1°", "0.8°", "0.5°", "0.4°", "0.3°", "0.2°" });
			((Control)cmbC2AFieldSize).set_Location(new Point(515, 71));
			((Control)cmbC2AFieldSize).set_Name("cmbC2AFieldSize");
			((Control)cmbC2AFieldSize).set_Size(new Size(61, 21));
			((Control)cmbC2AFieldSize).set_TabIndex(7);
			((Control)cmbC2AFieldSize).set_Text(Settings.Default.C2AFieldSize);
			((Control)label146).set_AutoSize(true);
			((Control)label146).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label146).set_Location(new Point(499, 54));
			((Control)label146).set_Name("label146");
			((Control)label146).set_Size(new Size(94, 13));
			((Control)label146).set_TabIndex(6);
			((Control)label146).set_Text("Width of star chart\r\n");
			((Control)label178).set_AutoSize(true);
			((Control)label178).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label178).set_Location(new Point(99, 58));
			((Control)label178).set_Name("label178");
			((Control)label178).set_Size(new Size(200, 13));
			((Control)label178).set_TabIndex(2);
			((Control)label178).set_Text("You need to specify the directory for C2A");
			((Control)cmdFolder_C2A).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFolder_C2A).set_Location(new Point(358, 74));
			((Control)cmdFolder_C2A).set_Name("cmdFolder_C2A");
			((Control)cmdFolder_C2A).set_Size(new Size(29, 20));
			((Control)cmdFolder_C2A).set_TabIndex(4);
			((Control)cmdFolder_C2A).set_Text("set");
			((ButtonBase)cmdFolder_C2A).set_UseVisualStyleBackColor(true);
			((Control)cmdFolder_C2A).add_Click((EventHandler)cmdFolder_C2A_Click);
			((Control)txtC2A).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "C2A_Path", true, (DataSourceUpdateMode)1));
			((Control)txtC2A).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtC2A).set_Location(new Point(99, 74));
			((Control)txtC2A).set_Name("txtC2A");
			((Control)txtC2A).set_Size(new Size(259, 20));
			((Control)txtC2A).set_TabIndex(3);
			((Control)txtC2A).set_Text(Settings.Default.C2A_Path);
			((Control)label177).set_AutoSize(true);
			((Control)label177).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label177).set_Location(new Point(6, 67));
			((Control)label177).set_Name("label177");
			((Control)label177).set_Size(new Size(89, 26));
			((Control)label177).set_TabIndex(1);
			((Control)label177).set_Text("C2A for Windows\r\nPath");
			label177.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)linkC2A).set_AutoSize(true);
			((Control)linkC2A).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)linkC2A).set_Location(new Point(49, 25));
			((Control)linkC2A).set_Name("linkC2A");
			((Control)linkC2A).set_Size(new Size(655, 13));
			((Control)linkC2A).set_TabIndex(0);
			linkC2A.set_TabStop(true);
			((Control)linkC2A).set_Text("Occult can draw star charts in the freeware Planetarium program C2A, which is downloadable from the C2A download web site (click here)");
			linkC2A.add_LinkClicked(new LinkLabelLinkClickedEventHandler(linkC2A_LinkClicked));
			((Control)grpEmails).set_BackColor(Color.AntiqueWhite);
			((Control)grpEmails).get_Controls().Add((Control)(object)grpEmailAdvanced);
			((Control)grpEmails).get_Controls().Add((Control)(object)FTPPassword);
			((Control)grpEmails).get_Controls().Add((Control)(object)label10);
			((Control)grpEmails).get_Controls().Add((Control)(object)label74);
			((Control)grpEmails).get_Controls().Add((Control)(object)label118);
			((Control)grpEmails).get_Controls().Add((Control)(object)textBox4);
			((Control)grpEmails).get_Controls().Add((Control)(object)label119);
			((Control)grpEmails).get_Controls().Add((Control)(object)label126);
			((Control)grpEmails).get_Controls().Add((Control)(object)label127);
			((Control)grpEmails).get_Controls().Add((Control)(object)numericUpDown57);
			((Control)grpEmails).get_Controls().Add((Control)(object)label128);
			((Control)grpEmails).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpEmails).set_Location(new Point(13, 494));
			((Control)grpEmails).set_Name("grpEmails");
			((Control)grpEmails).set_Size(new Size(758, 212));
			((Control)grpEmails).set_TabIndex(15);
			grpEmails.set_TabStop(false);
			((Control)grpEmails).set_Text("4. Email settings");
			((Control)grpTest).set_BackColor(Color.Pink);
			((Control)grpTest).get_Controls().Add((Control)(object)chkReposition);
			((Control)grpTest).get_Controls().Add((Control)(object)cmdPurgeLunarCache);
			((Control)grpTest).get_Controls().Add((Control)(object)chkForceVSOP87);
			((Control)grpTest).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpTest).set_Location(new Point(10, 4018));
			((Control)grpTest).set_Name("grpTest");
			((Control)grpTest).set_Size(new Size(758, 76));
			((Control)grpTest).set_TabIndex(14);
			grpTest.set_TabStop(false);
			((Control)grpTest).set_Text("16. Temporary changes - only apply to the current session");
			((Control)chkReposition).set_AutoSize(true);
			chkReposition.set_Checked(Settings.Default.RepositionAsteroidPrediction);
			((Control)chkReposition).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "RepositionAsteroidPrediction", true, (DataSourceUpdateMode)1));
			((Control)chkReposition).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkReposition).set_Location(new Point(22, 56));
			((Control)chkReposition).set_Name("chkReposition");
			((Control)chkReposition).set_Size(new Size(311, 17));
			((Control)chkReposition).set_TabIndex(2);
			((Control)chkReposition).set_Text("Allow repositioning of Prediction lines in Asteroidal reductions");
			((ButtonBase)chkReposition).set_UseVisualStyleBackColor(true);
			((Control)cmdPurgeLunarCache).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdPurgeLunarCache).set_Location(new Point(234, 25));
			((Control)cmdPurgeLunarCache).set_Name("cmdPurgeLunarCache");
			((Control)cmdPurgeLunarCache).set_Size(new Size(116, 22));
			((Control)cmdPurgeLunarCache).set_TabIndex(1);
			((Control)cmdPurgeLunarCache).set_Text("Purge Lunar Cache");
			((ButtonBase)cmdPurgeLunarCache).set_UseVisualStyleBackColor(true);
			((Control)cmdPurgeLunarCache).add_Click((EventHandler)cmdPurgeLunarCache_Click);
			((Control)chkForceVSOP87).set_AutoSize(true);
			((Control)chkForceVSOP87).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkForceVSOP87).set_Location(new Point(22, 25));
			((Control)chkForceVSOP87).set_Name("chkForceVSOP87");
			((Control)chkForceVSOP87).set_Size(new Size(180, 17));
			((Control)chkForceVSOP87).set_TabIndex(0);
			((Control)chkForceVSOP87).set_Text("Use VSOP87 for all ephemerides");
			((ButtonBase)chkForceVSOP87).set_UseVisualStyleBackColor(true);
			chkForceVSOP87.add_CheckedChanged((EventHandler)chkForceVSOP87_CheckedChanged);
			((Control)grpUpdates).set_BackColor(Color.AntiqueWhite);
			((Control)grpUpdates).get_Controls().Add((Control)(object)cmbLightCurveReminder);
			((Control)grpUpdates).get_Controls().Add((Control)(object)label173);
			((Control)grpUpdates).get_Controls().Add((Control)(object)label145);
			((Control)grpUpdates).get_Controls().Add((Control)(object)cmbUpdateFrequency);
			((Control)grpUpdates).get_Controls().Add((Control)(object)label144);
			((Control)grpUpdates).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpUpdates).set_Location(new Point(16, 193));
			((Control)grpUpdates).set_Name("grpUpdates");
			((Control)grpUpdates).set_Size(new Size(758, 76));
			((Control)grpUpdates).set_TabIndex(1);
			grpUpdates.set_TabStop(false);
			((Control)grpUpdates).set_Text("2. Check for availability of updates to Occult program and data files");
			cmbLightCurveReminder.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLightCurveReminder).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLightCurveReminder).set_FormattingEnabled(true);
			cmbLightCurveReminder.get_Items().AddRange(new object[5] { "Never", "Daily", "Weekly", "Monthly", "3-monthly" });
			((Control)cmbLightCurveReminder).set_Location(new Point(580, 43));
			((Control)cmbLightCurveReminder).set_Name("cmbLightCurveReminder");
			((Control)cmbLightCurveReminder).set_Size(new Size(88, 21));
			((Control)cmbLightCurveReminder).set_TabIndex(6);
			cmbLightCurveReminder.add_SelectedIndexChanged((EventHandler)cmbLightCurveReminder_SelectedIndexChanged);
			((Control)label173).set_AutoSize(true);
			((Control)label173).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label173).set_Location(new Point(420, 47));
			((Control)label173).set_Name("label173");
			((Control)label173).set_Size(new Size(160, 13));
			((Control)label173).set_TabIndex(5);
			((Control)label173).set_Text("Check for light curves to upload:");
			((Control)label145).set_AutoSize(true);
			((Control)label145).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label145).set_Location(new Point(53, 46));
			((Control)label145).set_Name("label145");
			((Control)label145).set_Size(new Size(191, 13));
			((Control)label145).set_TabIndex(4);
			((Control)label145).set_Text("Check for updates at program start-up: ");
			cmbUpdateFrequency.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbUpdateFrequency).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbUpdateFrequency).set_FormattingEnabled(true);
			cmbUpdateFrequency.get_Items().AddRange(new object[6] { "Every start up", "Daily", "Weekly", "Monthly", "Yearly", "Never" });
			((Control)cmbUpdateFrequency).set_Location(new Point(245, 43));
			((Control)cmbUpdateFrequency).set_Name("cmbUpdateFrequency");
			((Control)cmbUpdateFrequency).set_Size(new Size(94, 21));
			((Control)cmbUpdateFrequency).set_TabIndex(3);
			cmbUpdateFrequency.add_SelectedIndexChanged((EventHandler)cmbUpdateFrequency_SelectedIndexChanged);
			((Control)label144).set_AutoSize(true);
			((Control)label144).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label144).set_Location(new Point(6, 19));
			((Control)label144).set_Name("label144");
			((Control)label144).set_Size(new Size(546, 13));
			((Control)label144).set_TabIndex(2);
			((Control)label144).set_Text("Occult can check for the availability of progarm and data updates at start-up - if the computer has Internet access. ");
			((Control)grpAsteroidMultiPathMaps).set_BackColor(Color.AntiqueWhite);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)checkBox3);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label85);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)updnWest);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)updnEast);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)updnNorth);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)updnSouth);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label16);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label15);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label90);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label14);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label91);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label13);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label92);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown4);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label93);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown3);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label86);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown2);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label87);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown1);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label88);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown8);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label89);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown7);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown56);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown6);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown55);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown5);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown54);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown12);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown53);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown11);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown52);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown10);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown51);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown9);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown50);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label17);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown49);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label18);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown48);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label19);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown47);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)label20);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown46);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown41);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown45);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown42);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown44);
			((Control)grpAsteroidMultiPathMaps).get_Controls().Add((Control)(object)numericUpDown43);
			((Control)grpAsteroidMultiPathMaps).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpAsteroidMultiPathMaps).set_Location(new Point(13, 2856));
			((Control)grpAsteroidMultiPathMaps).set_Name("grpAsteroidMultiPathMaps");
			((Control)grpAsteroidMultiPathMaps).set_Size(new Size(758, 167));
			((Control)grpAsteroidMultiPathMaps).set_TabIndex(9);
			grpAsteroidMultiPathMaps.set_TabStop(false);
			((Control)grpAsteroidMultiPathMaps).set_Text("12. Asteroid Multipath maps;    Lunar graze maps");
			((Control)checkBox3).set_AutoSize(true);
			checkBox3.set_Checked(Settings.Default.MapRegionUseFirst);
			((Control)checkBox3).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "MapRegionUseFirst", true, (DataSourceUpdateMode)1));
			((Control)checkBox3).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox3).set_Location(new Point(283, 62));
			((Control)checkBox3).set_Name("checkBox3");
			((Control)checkBox3).set_Size(new Size(128, 30));
			((Control)checkBox3).set_TabIndex(10);
			((Control)checkBox3).set_Text("Use as initial region in\r\nlunar graze maps");
			((ButtonBase)checkBox3).set_UseVisualStyleBackColor(true);
			((Control)label85).set_AutoSize(true);
			((Control)label85).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label85).set_Location(new Point(10, 21));
			((Control)label85).set_Name("label85");
			((Control)label85).set_Size(new Size(735, 26));
			((Control)label85).set_TabIndex(0);
			((Control)label85).set_Text(componentResourceManager.GetString("label85.Text"));
			((Control)updnWest).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest1", true, (DataSourceUpdateMode)1));
			updnWest.set_DecimalPlaces(1);
			((Control)updnWest).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnWest).set_Location(new Point(32, 67));
			updnWest.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnWest.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnWest).set_Name("updnWest");
			((Control)updnWest).set_Size(new Size(65, 20));
			((Control)updnWest).set_TabIndex(6);
			updnWest.set_Value(Settings.Default.MapWest1);
			((Control)updnEast).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast1", true, (DataSourceUpdateMode)1));
			updnEast.set_DecimalPlaces(1);
			((Control)updnEast).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnEast).set_Location(new Point(100, 67));
			updnEast.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			updnEast.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)updnEast).set_Name("updnEast");
			((Control)updnEast).set_Size(new Size(65, 20));
			((Control)updnEast).set_TabIndex(7);
			updnEast.set_Value(Settings.Default.MapEast1);
			((Control)updnNorth).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth1", true, (DataSourceUpdateMode)1));
			updnNorth.set_DecimalPlaces(1);
			((Control)updnNorth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnNorth).set_Location(new Point(168, 67));
			updnNorth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnNorth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnNorth).set_Name("updnNorth");
			((Control)updnNorth).set_Size(new Size(52, 20));
			((Control)updnNorth).set_TabIndex(8);
			updnNorth.set_Value(Settings.Default.MapNorth1);
			((Control)updnSouth).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth1", true, (DataSourceUpdateMode)1));
			updnSouth.set_DecimalPlaces(1);
			((Control)updnSouth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnSouth).set_Location(new Point(223, 67));
			updnSouth.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			updnSouth.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)updnSouth).set_Name("updnSouth");
			((Control)updnSouth).set_Size(new Size(48, 20));
			((Control)updnSouth).set_TabIndex(9);
			updnSouth.set_Value(Settings.Default.MapSouth1);
			((Control)label90).set_AutoSize(true);
			((Control)label90).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label90).set_Location(new Point(645, 51));
			((Control)label90).set_Name("label90");
			((Control)label90).set_Size(new Size(35, 13));
			((Control)label90).set_TabIndex(29);
			((Control)label90).set_Text("South");
			((Control)label91).set_AutoSize(true);
			((Control)label91).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label91).set_Location(new Point(590, 51));
			((Control)label91).set_Name("label91");
			((Control)label91).set_Size(new Size(33, 13));
			((Control)label91).set_TabIndex(28);
			((Control)label91).set_Text("North");
			((Control)label92).set_AutoSize(true);
			((Control)label92).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label92).set_Location(new Point(522, 51));
			((Control)label92).set_Name("label92");
			((Control)label92).set_Size(new Size(28, 13));
			((Control)label92).set_TabIndex(27);
			((Control)label92).set_Text("East");
			((Control)numericUpDown4).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest2", true, (DataSourceUpdateMode)1));
			numericUpDown4.set_DecimalPlaces(1);
			((Control)numericUpDown4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown4).set_Location(new Point(32, 90));
			numericUpDown4.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown4.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown4).set_Name("numericUpDown4");
			((Control)numericUpDown4).set_Size(new Size(65, 20));
			((Control)numericUpDown4).set_TabIndex(12);
			numericUpDown4.set_Value(Settings.Default.MapWest2);
			((Control)label93).set_AutoSize(true);
			((Control)label93).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label93).set_Location(new Point(454, 51));
			((Control)label93).set_Name("label93");
			((Control)label93).set_Size(new Size(32, 13));
			((Control)label93).set_TabIndex(26);
			((Control)label93).set_Text("West");
			((Control)numericUpDown3).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast2", true, (DataSourceUpdateMode)1));
			numericUpDown3.set_DecimalPlaces(1);
			((Control)numericUpDown3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown3).set_Location(new Point(100, 90));
			numericUpDown3.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown3.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown3).set_Name("numericUpDown3");
			((Control)numericUpDown3).set_Size(new Size(65, 20));
			((Control)numericUpDown3).set_TabIndex(13);
			numericUpDown3.set_Value(Settings.Default.MapEast2);
			((Control)label86).set_AutoSize(true);
			((Control)label86).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label86).set_Location(new Point(432, 139));
			((Control)label86).set_Name("label86");
			((Control)label86).set_Size(new Size(20, 13));
			((Control)label86).set_TabIndex(45);
			((Control)label86).set_Text("#8");
			((Control)numericUpDown2).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth2", true, (DataSourceUpdateMode)1));
			numericUpDown2.set_DecimalPlaces(1);
			((Control)numericUpDown2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown2).set_Location(new Point(168, 90));
			numericUpDown2.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown2.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown2).set_Name("numericUpDown2");
			((Control)numericUpDown2).set_Size(new Size(52, 20));
			((Control)numericUpDown2).set_TabIndex(14);
			numericUpDown2.set_Value(Settings.Default.MapNorth2);
			((Control)label87).set_AutoSize(true);
			((Control)label87).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label87).set_Location(new Point(432, 116));
			((Control)label87).set_Name("label87");
			((Control)label87).set_Size(new Size(20, 13));
			((Control)label87).set_TabIndex(40);
			((Control)label87).set_Text("#7");
			((Control)numericUpDown1).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth2", true, (DataSourceUpdateMode)1));
			numericUpDown1.set_DecimalPlaces(1);
			((Control)numericUpDown1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown1).set_Location(new Point(223, 90));
			numericUpDown1.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown1.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown1).set_Name("numericUpDown1");
			((Control)numericUpDown1).set_Size(new Size(48, 20));
			((Control)numericUpDown1).set_TabIndex(15);
			numericUpDown1.set_Value(Settings.Default.MapSouth2);
			((Control)label88).set_AutoSize(true);
			((Control)label88).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label88).set_Location(new Point(432, 93));
			((Control)label88).set_Name("label88");
			((Control)label88).set_Size(new Size(20, 13));
			((Control)label88).set_TabIndex(35);
			((Control)label88).set_Text("#6");
			((Control)numericUpDown8).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest3", true, (DataSourceUpdateMode)1));
			numericUpDown8.set_DecimalPlaces(1);
			((Control)numericUpDown8).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown8).set_Location(new Point(32, 113));
			numericUpDown8.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown8.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown8).set_Name("numericUpDown8");
			((Control)numericUpDown8).set_Size(new Size(65, 20));
			((Control)numericUpDown8).set_TabIndex(17);
			numericUpDown8.set_Value(Settings.Default.MapWest3);
			((Control)label89).set_AutoSize(true);
			((Control)label89).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label89).set_Location(new Point(432, 70));
			((Control)label89).set_Name("label89");
			((Control)label89).set_Size(new Size(20, 13));
			((Control)label89).set_TabIndex(30);
			((Control)label89).set_Text("#5");
			((Control)numericUpDown7).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast3", true, (DataSourceUpdateMode)1));
			numericUpDown7.set_DecimalPlaces(1);
			((Control)numericUpDown7).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown7).set_Location(new Point(100, 113));
			numericUpDown7.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown7.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown7).set_Name("numericUpDown7");
			((Control)numericUpDown7).set_Size(new Size(65, 20));
			((Control)numericUpDown7).set_TabIndex(18);
			numericUpDown7.set_Value(Settings.Default.MapEast3);
			((Control)numericUpDown56).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth6", true, (DataSourceUpdateMode)1));
			numericUpDown56.set_DecimalPlaces(1);
			((Control)numericUpDown56).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown56).set_Location(new Point(590, 90));
			numericUpDown56.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown56.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown56).set_Name("numericUpDown56");
			((Control)numericUpDown56).set_Size(new Size(52, 20));
			((Control)numericUpDown56).set_TabIndex(38);
			numericUpDown56.set_Value(Settings.Default.MapNorth6);
			((Control)numericUpDown6).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth3", true, (DataSourceUpdateMode)1));
			numericUpDown6.set_DecimalPlaces(1);
			((Control)numericUpDown6).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown6).set_Location(new Point(168, 113));
			numericUpDown6.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown6.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown6).set_Name("numericUpDown6");
			((Control)numericUpDown6).set_Size(new Size(52, 20));
			((Control)numericUpDown6).set_TabIndex(19);
			numericUpDown6.set_Value(Settings.Default.MapNorth3);
			((Control)numericUpDown55).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth7", true, (DataSourceUpdateMode)1));
			numericUpDown55.set_DecimalPlaces(1);
			((Control)numericUpDown55).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown55).set_Location(new Point(590, 113));
			numericUpDown55.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown55.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown55).set_Name("numericUpDown55");
			((Control)numericUpDown55).set_Size(new Size(52, 20));
			((Control)numericUpDown55).set_TabIndex(43);
			numericUpDown55.set_Value(Settings.Default.MapNorth7);
			((Control)numericUpDown5).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth3", true, (DataSourceUpdateMode)1));
			numericUpDown5.set_DecimalPlaces(1);
			((Control)numericUpDown5).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown5).set_Location(new Point(223, 113));
			numericUpDown5.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown5.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown5).set_Name("numericUpDown5");
			((Control)numericUpDown5).set_Size(new Size(48, 20));
			((Control)numericUpDown5).set_TabIndex(20);
			numericUpDown5.set_Value(Settings.Default.MapSouth3);
			((Control)numericUpDown54).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth8", true, (DataSourceUpdateMode)1));
			numericUpDown54.set_DecimalPlaces(1);
			((Control)numericUpDown54).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown54).set_Location(new Point(590, 136));
			numericUpDown54.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown54.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown54).set_Name("numericUpDown54");
			((Control)numericUpDown54).set_Size(new Size(52, 20));
			((Control)numericUpDown54).set_TabIndex(48);
			numericUpDown54.set_Value(Settings.Default.MapNorth8);
			((Control)numericUpDown12).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest4", true, (DataSourceUpdateMode)1));
			numericUpDown12.set_DecimalPlaces(1);
			((Control)numericUpDown12).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown12).set_Location(new Point(32, 136));
			numericUpDown12.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown12.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown12).set_Name("numericUpDown12");
			((Control)numericUpDown12).set_Size(new Size(65, 20));
			((Control)numericUpDown12).set_TabIndex(22);
			numericUpDown12.set_Value(Settings.Default.MapWest4);
			((Control)numericUpDown53).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth5", true, (DataSourceUpdateMode)1));
			numericUpDown53.set_DecimalPlaces(1);
			((Control)numericUpDown53).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown53).set_Location(new Point(645, 67));
			numericUpDown53.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown53.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown53).set_Name("numericUpDown53");
			((Control)numericUpDown53).set_Size(new Size(52, 20));
			((Control)numericUpDown53).set_TabIndex(34);
			numericUpDown53.set_Value(Settings.Default.MapSouth5);
			((Control)numericUpDown11).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast4", true, (DataSourceUpdateMode)1));
			numericUpDown11.set_DecimalPlaces(1);
			((Control)numericUpDown11).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown11).set_Location(new Point(100, 136));
			numericUpDown11.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown11.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown11).set_Name("numericUpDown11");
			((Control)numericUpDown11).set_Size(new Size(65, 20));
			((Control)numericUpDown11).set_TabIndex(23);
			numericUpDown11.set_Value(Settings.Default.MapEast4);
			((Control)numericUpDown52).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth6", true, (DataSourceUpdateMode)1));
			numericUpDown52.set_DecimalPlaces(1);
			((Control)numericUpDown52).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown52).set_Location(new Point(645, 90));
			numericUpDown52.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown52.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown52).set_Name("numericUpDown52");
			((Control)numericUpDown52).set_Size(new Size(52, 20));
			((Control)numericUpDown52).set_TabIndex(39);
			numericUpDown52.set_Value(Settings.Default.MapSouth6);
			((Control)numericUpDown10).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth4", true, (DataSourceUpdateMode)1));
			numericUpDown10.set_DecimalPlaces(1);
			((Control)numericUpDown10).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown10).set_Location(new Point(168, 136));
			numericUpDown10.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown10.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown10).set_Name("numericUpDown10");
			((Control)numericUpDown10).set_Size(new Size(52, 20));
			((Control)numericUpDown10).set_TabIndex(24);
			numericUpDown10.set_Value(Settings.Default.MapNorth4);
			((Control)numericUpDown51).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth7", true, (DataSourceUpdateMode)1));
			numericUpDown51.set_DecimalPlaces(1);
			((Control)numericUpDown51).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown51).set_Location(new Point(645, 113));
			numericUpDown51.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown51.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown51).set_Name("numericUpDown51");
			((Control)numericUpDown51).set_Size(new Size(52, 20));
			((Control)numericUpDown51).set_TabIndex(44);
			numericUpDown51.set_Value(Settings.Default.MapSouth7);
			((Control)numericUpDown9).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth4", true, (DataSourceUpdateMode)1));
			numericUpDown9.set_DecimalPlaces(1);
			((Control)numericUpDown9).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown9).set_Location(new Point(223, 136));
			numericUpDown9.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown9.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown9).set_Name("numericUpDown9");
			((Control)numericUpDown9).set_Size(new Size(48, 20));
			((Control)numericUpDown9).set_TabIndex(25);
			numericUpDown9.set_Value(Settings.Default.MapSouth4);
			((Control)numericUpDown50).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapSouth8", true, (DataSourceUpdateMode)1));
			numericUpDown50.set_DecimalPlaces(1);
			((Control)numericUpDown50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown50).set_Location(new Point(645, 136));
			numericUpDown50.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown50.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown50).set_Name("numericUpDown50");
			((Control)numericUpDown50).set_Size(new Size(52, 20));
			((Control)numericUpDown50).set_TabIndex(49);
			numericUpDown50.set_Value(Settings.Default.MapSouth8);
			((Control)numericUpDown49).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest6", true, (DataSourceUpdateMode)1));
			numericUpDown49.set_DecimalPlaces(1);
			((Control)numericUpDown49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown49).set_Location(new Point(454, 90));
			numericUpDown49.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown49.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown49).set_Name("numericUpDown49");
			((Control)numericUpDown49).set_Size(new Size(65, 20));
			((Control)numericUpDown49).set_TabIndex(36);
			numericUpDown49.set_Value(Settings.Default.MapWest6);
			((Control)numericUpDown48).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest7", true, (DataSourceUpdateMode)1));
			numericUpDown48.set_DecimalPlaces(1);
			((Control)numericUpDown48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown48).set_Location(new Point(454, 113));
			numericUpDown48.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown48.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown48).set_Name("numericUpDown48");
			((Control)numericUpDown48).set_Size(new Size(65, 20));
			((Control)numericUpDown48).set_TabIndex(41);
			numericUpDown48.set_Value(Settings.Default.MapWest7);
			((Control)numericUpDown47).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest8", true, (DataSourceUpdateMode)1));
			numericUpDown47.set_DecimalPlaces(1);
			((Control)numericUpDown47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown47).set_Location(new Point(454, 136));
			numericUpDown47.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown47.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown47).set_Name("numericUpDown47");
			((Control)numericUpDown47).set_Size(new Size(65, 20));
			((Control)numericUpDown47).set_TabIndex(46);
			numericUpDown47.set_Value(Settings.Default.MapWest8);
			((Control)numericUpDown46).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast5", true, (DataSourceUpdateMode)1));
			numericUpDown46.set_DecimalPlaces(1);
			((Control)numericUpDown46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown46).set_Location(new Point(522, 67));
			numericUpDown46.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown46.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown46).set_Name("numericUpDown46");
			((Control)numericUpDown46).set_Size(new Size(65, 20));
			((Control)numericUpDown46).set_TabIndex(32);
			numericUpDown46.set_Value(Settings.Default.MapEast5);
			((Control)numericUpDown41).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapWest5", true, (DataSourceUpdateMode)1));
			numericUpDown41.set_DecimalPlaces(1);
			((Control)numericUpDown41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown41).set_Location(new Point(454, 67));
			numericUpDown41.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown41.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown41).set_Name("numericUpDown41");
			((Control)numericUpDown41).set_Size(new Size(65, 20));
			((Control)numericUpDown41).set_TabIndex(31);
			numericUpDown41.set_Value(Settings.Default.MapWest5);
			((Control)numericUpDown45).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast6", true, (DataSourceUpdateMode)1));
			numericUpDown45.set_DecimalPlaces(1);
			((Control)numericUpDown45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown45).set_Location(new Point(522, 90));
			numericUpDown45.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown45.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown45).set_Name("numericUpDown45");
			((Control)numericUpDown45).set_Size(new Size(65, 20));
			((Control)numericUpDown45).set_TabIndex(37);
			numericUpDown45.set_Value(Settings.Default.MapEast6);
			((Control)numericUpDown42).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapNorth5", true, (DataSourceUpdateMode)1));
			numericUpDown42.set_DecimalPlaces(1);
			((Control)numericUpDown42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown42).set_Location(new Point(590, 67));
			numericUpDown42.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown42.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown42).set_Name("numericUpDown42");
			((Control)numericUpDown42).set_Size(new Size(52, 20));
			((Control)numericUpDown42).set_TabIndex(33);
			numericUpDown42.set_Value(Settings.Default.MapNorth5);
			((Control)numericUpDown44).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast7", true, (DataSourceUpdateMode)1));
			numericUpDown44.set_DecimalPlaces(1);
			((Control)numericUpDown44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown44).set_Location(new Point(522, 113));
			numericUpDown44.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown44.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown44).set_Name("numericUpDown44");
			((Control)numericUpDown44).set_Size(new Size(65, 20));
			((Control)numericUpDown44).set_TabIndex(42);
			numericUpDown44.set_Value(Settings.Default.MapEast7);
			((Control)numericUpDown43).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MapEast8", true, (DataSourceUpdateMode)1));
			numericUpDown43.set_DecimalPlaces(1);
			((Control)numericUpDown43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown43).set_Location(new Point(522, 136));
			numericUpDown43.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown43.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown43).set_Name("numericUpDown43");
			((Control)numericUpDown43).set_Size(new Size(65, 20));
			((Control)numericUpDown43).set_TabIndex(47);
			numericUpDown43.set_Value(Settings.Default.MapEast8);
			((Control)grpLunarReports).set_BackColor(Color.AntiqueWhite);
			((Control)grpLunarReports).get_Controls().Add((Control)(object)updnEOPReminder);
			((Control)grpLunarReports).get_Controls().Add((Control)(object)label132);
			((Control)grpLunarReports).get_Controls().Add((Control)(object)updnCentury);
			((Control)grpLunarReports).get_Controls().Add((Control)(object)label68);
			((Control)grpLunarReports).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpLunarReports).set_Location(new Point(13, 3657));
			((Control)grpLunarReports).set_Name("grpLunarReports");
			((Control)grpLunarReports).set_Size(new Size(758, 120));
			((Control)grpLunarReports).set_TabIndex(11);
			grpLunarReports.set_TabStop(false);
			((Control)grpLunarReports).set_Text("14. Lunar occultation - Reports and Reductions");
			((Control)updnEOPReminder).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "EOPReminder", true, (DataSourceUpdateMode)1));
			((Control)updnEOPReminder).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnEOPReminder.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnEOPReminder).set_Location(new Point(516, 28));
			updnEOPReminder.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnEOPReminder).set_Name("updnEOPReminder");
			((Control)updnEOPReminder).set_Size(new Size(42, 20));
			((Control)updnEOPReminder).set_TabIndex(1);
			updnEOPReminder.set_Value(Settings.Default.EOPReminder);
			((Control)label132).set_AutoSize(true);
			((Control)label132).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label132).set_Location(new Point(26, 26));
			((Control)label132).set_Name("label132");
			((Control)label132).set_Size(new Size(461, 26));
			((Control)label132).set_TabIndex(0);
			((Control)label132).set_Text("For accurate reductions of recent observations, the file of Earth Orientation Parameters needs to \r\nbe current. A warning will be given if the file is more than these number of days old.");
			((Control)updnCentury).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "ReportCentury", true, (DataSourceUpdateMode)1));
			((Control)updnCentury).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnCentury).set_Location(new Point(361, 74));
			updnCentury.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnCentury.set_Minimum(new decimal(new int[4] { 16, 0, 0, 0 }));
			((Control)updnCentury).set_Name("updnCentury");
			((Control)updnCentury).set_Size(new Size(42, 20));
			((Control)updnCentury).set_TabIndex(3);
			updnCentury.set_Value(Settings.Default.ReportCentury);
			((Control)label68).set_AutoSize(true);
			((Control)label68).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label68).set_Location(new Point(25, 65));
			((Control)label68).set_Name("label68");
			((Control)label68).set_Size(new Size(299, 39));
			((Control)label68).set_TabIndex(2);
			((Control)label68).set_Text("When reading an 'old' occultation report using certain other\r\n formats (eg the '76 format'), Occult needs to insert the century\r\n year into the date. The century year is set here.");
			((Control)grpAdministrator).set_BackColor(Color.Pink);
			((Control)grpAdministrator).get_Controls().Add((Control)(object)chkLunarGlobal);
			((Control)grpAdministrator).get_Controls().Add((Control)(object)chkLunarRegional);
			((Control)grpAdministrator).get_Controls().Add((Control)(object)checkBox2);
			((Control)grpAdministrator).get_Controls().Add((Control)(object)label97);
			((Control)grpAdministrator).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpAdministrator).set_Location(new Point(13, 3812));
			((Control)grpAdministrator).set_Name("grpAdministrator");
			((Control)grpAdministrator).set_Size(new Size(758, 181));
			((Control)grpAdministrator).set_TabIndex(12);
			grpAdministrator.set_TabStop(false);
			((Control)grpAdministrator).set_Text("15. Administrator functionalities");
			((Control)chkLunarGlobal).set_AutoSize(true);
			chkLunarGlobal.set_Checked(Settings.Default.AdministratorGlobal);
			((Control)chkLunarGlobal).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AdministratorGlobal", true, (DataSourceUpdateMode)1));
			((Control)chkLunarGlobal).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLunarGlobal).set_Location(new Point(15, 152));
			((Control)chkLunarGlobal).set_Name("chkLunarGlobal");
			((Control)chkLunarGlobal).set_Size(new Size(426, 17));
			((Control)chkLunarGlobal).set_TabIndex(3);
			((Control)chkLunarGlobal).set_Text("Lunar occultation observations - provide access as a Global collector of observations");
			((ButtonBase)chkLunarGlobal).set_UseVisualStyleBackColor(true);
			((Control)chkLunarRegional).set_AutoSize(true);
			chkLunarRegional.set_Checked(Settings.Default.AdministratorRegional);
			((Control)chkLunarRegional).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AdministratorRegional", true, (DataSourceUpdateMode)1));
			((Control)chkLunarRegional).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLunarRegional).set_Location(new Point(15, 127));
			((Control)chkLunarRegional).set_Name("chkLunarRegional");
			((Control)chkLunarRegional).set_Size(new Size(438, 17));
			((Control)chkLunarRegional).set_TabIndex(2);
			((Control)chkLunarRegional).set_Text("Lunar occultation observations - provide access as a Regional collector of observations");
			((ButtonBase)chkLunarRegional).set_UseVisualStyleBackColor(true);
			((Control)checkBox2).set_AutoSize(true);
			checkBox2.set_Checked(Settings.Default.Administrator);
			((Control)checkBox2).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Administrator", true, (DataSourceUpdateMode)1));
			((Control)checkBox2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox2).set_Location(new Point(15, 102));
			((Control)checkBox2).set_Name("checkBox2");
			((Control)checkBox2).set_Size(new Size(210, 17));
			((Control)checkBox2).set_TabIndex(1);
			((Control)checkBox2).set_Text("Enable general 'Administrator' functions");
			((ButtonBase)checkBox2).set_UseVisualStyleBackColor(true);
			((Control)label97).set_AutoSize(true);
			((Control)label97).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label97).set_Location(new Point(12, 25));
			((Control)label97).set_Name("label97");
			((Control)label97).set_Size(new Size(709, 65));
			((Control)label97).set_TabIndex(0);
			((Control)label97).set_Text(componentResourceManager.GetString("label97.Text"));
			((Control)grpSiteFiles).set_BackColor(Color.AntiqueWhite);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)label81);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)label40);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)cmbSortOrder);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)cmbSitePlot);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)label39);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)updnMagCorrection);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)label38);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)label37);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)updnTelescopeAperture);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)label36);
			((Control)grpSiteFiles).get_Controls().Add((Control)(object)updnGrazeTtravelDistance);
			((Control)grpSiteFiles).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpSiteFiles).set_Location(new Point(13, 312));
			((Control)grpSiteFiles).set_Name("grpSiteFiles");
			((Control)grpSiteFiles).set_Size(new Size(758, 148));
			((Control)grpSiteFiles).set_TabIndex(2);
			grpSiteFiles.set_TabStop(false);
			((Control)grpSiteFiles).set_Text("3. Site Files ");
			((Control)label81).set_AutoSize(true);
			((Control)label81).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label81).set_Location(new Point(18, 23));
			((Control)label81).set_Name("label81");
			((Control)label81).set_Size(new Size(623, 26));
			((Control)label81).set_TabIndex(0);
			((Control)label81).set_Text("Occult uses 'site files' to control  a number of prediction parameters according to the site. The following settings are used as default \r\nvalues in the Site Editor.");
			((Control)label40).set_AutoSize(true);
			((Control)label40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label40).set_Location(new Point(47, 66));
			((Control)label40).set_Name("label40");
			((Control)label40).set_Size(new Size(134, 13));
			((Control)label40).set_TabIndex(1);
			((Control)label40).set_Text("Initial order of sites in editor");
			cmbSortOrder.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbSortOrder).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSortOrder).set_FormattingEnabled(true);
			cmbSortOrder.get_Items().AddRange(new object[3] { "Name", "Longitude", "Latitude" });
			((Control)cmbSortOrder).set_Location(new Point(188, 63));
			((Control)cmbSortOrder).set_Name("cmbSortOrder");
			((Control)cmbSortOrder).set_Size(new Size(102, 21));
			((Control)cmbSortOrder).set_TabIndex(2);
			cmbSortOrder.add_SelectedIndexChanged((EventHandler)cmbSortOrder_SelectedIndexChanged);
			cmbSitePlot.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbSitePlot).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSitePlot).set_FormattingEnabled(true);
			cmbSitePlot.get_Items().AddRange(new object[5] { "Never", "only on Detailed maps", "on Medium maps", "on All maps", "mobile site" });
			((Control)cmbSitePlot).set_Location(new Point(188, 115));
			((Control)cmbSitePlot).set_Name("cmbSitePlot");
			((Control)cmbSitePlot).set_Size(new Size(135, 21));
			((Control)cmbSitePlot).set_TabIndex(6);
			cmbSitePlot.add_SelectedIndexChanged((EventHandler)cmbSitePlot_SelectedIndexChanged);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label39).set_Location(new Point(42, 118));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(139, 13));
			((Control)label39).set_TabIndex(5);
			((Control)label39).set_Text("Criterion to plot site on maps");
			((Control)updnMagCorrection).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Site_MagnitudeCorrection", true, (DataSourceUpdateMode)1));
			updnMagCorrection.set_DecimalPlaces(1);
			((Control)updnMagCorrection).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMagCorrection).set_Location(new Point(630, 114));
			updnMagCorrection.set_Maximum(new decimal(new int[4] { 2, 0, 0, 0 }));
			updnMagCorrection.set_Minimum(new decimal(new int[4] { 1, 0, 0, -2147483648 }));
			((Control)updnMagCorrection).set_Name("updnMagCorrection");
			((Control)updnMagCorrection).set_Size(new Size(52, 20));
			((Control)updnMagCorrection).set_TabIndex(12);
			((UpDownBase)updnMagCorrection).set_TextAlign((HorizontalAlignment)2);
			updnMagCorrection.set_Value(Settings.Default.Site_MagnitudeCorrection);
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label38).set_Location(new Point(407, 119));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(215, 13));
			((Control)label38).set_TabIndex(11);
			((Control)label38).set_Text("Correction to limiting magnitude [Lunar occs]");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label37).set_Location(new Point(500, 67));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(122, 13));
			((Control)label37).set_TabIndex(7);
			((Control)label37).set_Text("Telescope aperture (cm)");
			((Control)updnTelescopeAperture).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Site_TelescopeAperture_cm", true, (DataSourceUpdateMode)1));
			((Control)updnTelescopeAperture).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnTelescopeAperture.set_Increment(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnTelescopeAperture).set_Location(new Point(630, 65));
			updnTelescopeAperture.set_Maximum(new decimal(new int[4] { 50, 0, 0, 0 }));
			updnTelescopeAperture.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnTelescopeAperture).set_Name("updnTelescopeAperture");
			((Control)updnTelescopeAperture).set_Size(new Size(52, 20));
			((Control)updnTelescopeAperture).set_TabIndex(8);
			((UpDownBase)updnTelescopeAperture).set_TextAlign((HorizontalAlignment)2);
			updnTelescopeAperture.set_Value(Settings.Default.Site_TelescopeAperture_cm);
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label36).set_Location(new Point(442, 92));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(178, 13));
			((Control)label36).set_TabIndex(9);
			((Control)label36).set_Text("Travel distance for lunar grazes (km)");
			((Control)updnGrazeTtravelDistance).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Site_GrazeTravelDisance", true, (DataSourceUpdateMode)1));
			((Control)updnGrazeTtravelDistance).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnGrazeTtravelDistance.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGrazeTtravelDistance).set_Location(new Point(630, 88));
			updnGrazeTtravelDistance.set_Maximum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnGrazeTtravelDistance.set_Minimum(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGrazeTtravelDistance).set_Name("updnGrazeTtravelDistance");
			((Control)updnGrazeTtravelDistance).set_Size(new Size(52, 20));
			((Control)updnGrazeTtravelDistance).set_TabIndex(10);
			((UpDownBase)updnGrazeTtravelDistance).set_TextAlign((HorizontalAlignment)2);
			updnGrazeTtravelDistance.set_Value(Settings.Default.Site_GrazeTravelDisance);
			((Control)grpFileSaveGraphics).set_BackColor(Color.AntiqueWhite);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)updnFontSize);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)label131);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)updnLineThickness);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)label130);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)chkSmoothing);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)label114);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)label80);
			((Control)grpFileSaveGraphics).get_Controls().Add((Control)(object)cmbGraphicSaveType);
			((Control)grpFileSaveGraphics).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpFileSaveGraphics).set_Location(new Point(13, 1836));
			((Control)grpFileSaveGraphics).set_Name("grpFileSaveGraphics");
			((Control)grpFileSaveGraphics).set_Size(new Size(758, 131));
			((Control)grpFileSaveGraphics).set_TabIndex(6);
			grpFileSaveGraphics.set_TabStop(false);
			((Control)grpFileSaveGraphics).set_Text("8. Graphics");
			((Control)updnFontSize).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsterPlot_LargeFontSize", true, (DataSourceUpdateMode)1));
			updnFontSize.set_DecimalPlaces(1);
			((Control)updnFontSize).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnFontSize.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnFontSize).set_Location(new Point(370, 103));
			updnFontSize.set_Maximum(new decimal(new int[4] { 30, 0, 0, 0 }));
			updnFontSize.set_Minimum(new decimal(new int[4] { 8, 0, 0, 0 }));
			((Control)updnFontSize).set_Name("updnFontSize");
			((Control)updnFontSize).set_Size(new Size(48, 20));
			((Control)updnFontSize).set_TabIndex(7);
			updnFontSize.set_Value(Settings.Default.AsterPlot_LargeFontSize);
			((Control)label131).set_AutoSize(true);
			((Control)label131).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label131).set_Location(new Point(139, 105));
			((Control)label131).set_Name("label131");
			((Control)label131).set_Size(new Size(225, 13));
			((Control)label131).set_TabIndex(6);
			((Control)label131).set_Text("... Font size when 'Large label font' option set. ");
			((Control)updnLineThickness).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsterPlot_LineThickness_Path", true, (DataSourceUpdateMode)1));
			updnLineThickness.set_DecimalPlaces(1);
			((Control)updnLineThickness).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnLineThickness.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnLineThickness).set_Location(new Point(395, 77));
			updnLineThickness.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnLineThickness.set_Minimum(new decimal(new int[4] { 15, 0, 0, 65536 }));
			((Control)updnLineThickness).set_Name("updnLineThickness");
			((Control)updnLineThickness).set_Size(new Size(45, 20));
			((Control)updnLineThickness).set_TabIndex(5);
			updnLineThickness.set_Value(Settings.Default.AsterPlot_LineThickness_Path);
			((Control)label130).set_AutoSize(true);
			((Control)label130).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label130).set_Location(new Point(11, 80));
			((Control)label130).set_Name("label130");
			((Control)label130).set_Size(new Size(381, 13));
			((Control)label130).set_TabIndex(4);
			((Control)label130).set_Text("Plot of asteroid observations. Thickness of the line when 'Thick line' option set. ");
			((Control)chkSmoothing).set_AutoSize(true);
			chkSmoothing.set_CheckAlign(ContentAlignment.MiddleRight);
			chkSmoothing.set_Checked(Settings.Default.GraphicsSmoothed);
			chkSmoothing.set_CheckState((CheckState)1);
			((Control)chkSmoothing).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "GraphicsSmoothed", true, (DataSourceUpdateMode)1));
			((Control)chkSmoothing).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkSmoothing).set_Location(new Point(11, 53));
			((Control)chkSmoothing).set_Name("chkSmoothing");
			((Control)chkSmoothing).set_Size(new Size(203, 17));
			((Control)chkSmoothing).set_TabIndex(3);
			((Control)chkSmoothing).set_Text("Use smoothing when drawing graphic");
			((ButtonBase)chkSmoothing).set_UseVisualStyleBackColor(true);
			((Control)label114).set_AutoSize(true);
			((Control)label114).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label114).set_Location(new Point(472, 18));
			((Control)label114).set_Name("label114");
			((Control)label114).set_Size(new Size(275, 26));
			((Control)label114).set_TabIndex(2);
			((Control)label114).set_Text("Format for AutoGenerated maps in Asteroid occultations -\r\nsee Asteroid occultations - search and display (below).");
			((Control)label80).set_AutoSize(true);
			((Control)label80).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label80).set_Location(new Point(11, 26));
			((Control)label80).set_Name("label80");
			((Control)label80).set_Size(new Size(366, 13));
			((Control)label80).set_TabIndex(0);
			((Control)label80).set_Text("Occult can save graphics in several formats. Specify the default format here.");
			cmbGraphicSaveType.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbGraphicSaveType).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbGraphicSaveType).set_FormattingEnabled(true);
			cmbGraphicSaveType.get_Items().AddRange(new object[5] { "JPEG", "BMP", "GIF", "PNG", "TIFF" });
			((Control)cmbGraphicSaveType).set_Location(new Point(380, 23));
			((Control)cmbGraphicSaveType).set_Name("cmbGraphicSaveType");
			((Control)cmbGraphicSaveType).set_Size(new Size(60, 21));
			((Control)cmbGraphicSaveType).set_TabIndex(1);
			cmbGraphicSaveType.add_SelectedIndexChanged((EventHandler)cmbGraphicSaveType_SelectedIndexChanged);
			((Control)grpAsteroidSearchDisplay).set_BackColor(Color.AntiqueWhite);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)panel2);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnMaximumSearchEvents);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label182);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkAddSAO);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)checkBox6);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label113);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)cmbAutoFileType);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label108);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkBessel);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkColour);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label84);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnTitanDiameter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label41);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkHTM);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnTritonDiameter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkGoogleEarth);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label78);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkPrepoint);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnPlutoDiameter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkMultisite);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label77);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkPathCoords);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkStarChart);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label63);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkWorld);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnUranusDiameter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)chkAutoGenerate);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label61);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnJupiterDiameter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnSaturnDiameter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label62);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnUncertainty);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label83);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label12);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnNeptuneDiameter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnUranus);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label60);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label27);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label59);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label28);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label58);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnMars);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label57);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label29);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnJupiter);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label30);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnNeptune);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label31);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label32);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnPluto);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label33);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label34);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnSaturn);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)label35);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnVenus);
			((Control)grpAsteroidSearchDisplay).get_Controls().Add((Control)(object)updnMercury);
			((Control)grpAsteroidSearchDisplay).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpAsteroidSearchDisplay).set_Location(new Point(13, 2348));
			((Control)grpAsteroidSearchDisplay).set_Name("grpAsteroidSearchDisplay");
			((Control)grpAsteroidSearchDisplay).set_Size(new Size(758, 461));
			((Control)grpAsteroidSearchDisplay).set_TabIndex(8);
			grpAsteroidSearchDisplay.set_TabStop(false);
			((Control)grpAsteroidSearchDisplay).set_Text("11. Asteroid occultations - search and display");
			panel2.set_BorderStyle((BorderStyle)1);
			((Control)panel2).get_Controls().Add((Control)(object)label183);
			((Control)panel2).get_Controls().Add((Control)(object)updnPlutoDecCorrn);
			((Control)panel2).get_Controls().Add((Control)(object)updnPlutoRACorrn);
			((Control)panel2).get_Controls().Add((Control)(object)label71);
			((Control)panel2).get_Controls().Add((Control)(object)label69);
			((Control)panel2).get_Controls().Add((Control)(object)label70);
			((Control)panel2).get_Controls().Add((Control)(object)label79);
			((Control)panel2).get_Controls().Add((Control)(object)label23);
			((Control)panel2).get_Controls().Add((Control)(object)updnDAteRangeInAsteroidASelection);
			((Control)panel2).get_Controls().Add((Control)(object)chkExportElements);
			((Control)panel2).set_Location(new Point(19, 143));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(683, 63));
			((Control)panel2).set_TabIndex(68);
			((Control)label183).set_AutoSize(true);
			((Control)label183).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label183).set_Location(new Point(405, 3));
			((Control)label183).set_Name("label183");
			((Control)label183).set_Size(new Size(238, 13));
			((Control)label183).set_TabIndex(64);
			((Control)label183).set_Text("As of 2018, these settings should not  be needed");
			((Control)updnPlutoDecCorrn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnPlutoDecCorrn).set_ForeColor(Color.DimGray);
			((Control)updnPlutoDecCorrn).set_Location(new Point(579, 40));
			updnPlutoDecCorrn.set_Maximum(new decimal(new int[4] { 999, 0, 0, 0 }));
			updnPlutoDecCorrn.set_Minimum(new decimal(new int[4] { 999, 0, 0, -2147483648 }));
			((Control)updnPlutoDecCorrn).set_Name("updnPlutoDecCorrn");
			((Control)updnPlutoDecCorrn).set_Size(new Size(41, 20));
			((Control)updnPlutoDecCorrn).set_TabIndex(63);
			((UpDownBase)updnPlutoDecCorrn).set_TextAlign((HorizontalAlignment)1);
			((Control)updnPlutoRACorrn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnPlutoRACorrn).set_ForeColor(Color.DimGray);
			((Control)updnPlutoRACorrn).set_Location(new Point(493, 40));
			updnPlutoRACorrn.set_Maximum(new decimal(new int[4] { 999, 0, 0, 0 }));
			updnPlutoRACorrn.set_Minimum(new decimal(new int[4] { 999, 0, 0, -2147483648 }));
			((Control)updnPlutoRACorrn).set_Name("updnPlutoRACorrn");
			((Control)updnPlutoRACorrn).set_Size(new Size(41, 20));
			((Control)updnPlutoRACorrn).set_TabIndex(62);
			((UpDownBase)updnPlutoRACorrn).set_TextAlign((HorizontalAlignment)1);
			((Control)label71).set_AutoSize(true);
			((Control)label71).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label71).set_ForeColor(Color.DimGray);
			((Control)label71).set_Location(new Point(466, 42));
			((Control)label71).set_Name("label71");
			((Control)label71).set_Size(new Size(28, 13));
			((Control)label71).set_TabIndex(9);
			((Control)label71).set_Text("R.A.");
			((Control)label69).set_AutoSize(true);
			((Control)label69).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label69).set_ForeColor(Color.DimGray);
			((Control)label69).set_Location(new Point(551, 42));
			((Control)label69).set_Name("label69");
			((Control)label69).set_Size(new Size(27, 13));
			((Control)label69).set_TabIndex(11);
			((Control)label69).set_Text("Dec");
			((Control)label70).set_AutoSize(true);
			((Control)label70).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label70).set_ForeColor(Color.DimGray);
			((Control)label70).set_Location(new Point(5, 42));
			((Control)label70).set_Name("label70");
			((Control)label70).set_Size(new Size(445, 13));
			((Control)label70).set_TabIndex(8);
			((Control)label70).set_Text("When searching for occultations by Pluto or its moons, default correction (Pluto - Star)  in mas");
			((Control)label79).set_AutoSize(true);
			((Control)label79).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label79).set_ForeColor(Color.DimGray);
			((Control)label79).set_Location(new Point(322, 5));
			((Control)label79).set_Name("label79");
			((Control)label79).set_Size(new Size(29, 13));
			((Control)label79).set_TabIndex(6);
			((Control)label79).set_Text("days");
			((Control)label23).set_AutoSize(true);
			((Control)label23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label23).set_ForeColor(Color.DimGray);
			((Control)label23).set_Location(new Point(7, 5));
			((Control)label23).set_Name("label23");
			((Control)label23).set_Size(new Size(263, 13));
			((Control)label23).set_TabIndex(4);
			((Control)label23).set_Text("Default date range in Display Asteroid selection criteria");
			((Control)updnDAteRangeInAsteroidASelection).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "DateRangeInAsteroidDisplaySelectionCriterion", true, (DataSourceUpdateMode)1));
			((Control)updnDAteRangeInAsteroidASelection).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnDAteRangeInAsteroidASelection).set_ForeColor(Color.DimGray);
			((Control)updnDAteRangeInAsteroidASelection).set_Location(new Point(276, 3));
			updnDAteRangeInAsteroidASelection.set_Maximum(new decimal(new int[4] { 366, 0, 0, 0 }));
			updnDAteRangeInAsteroidASelection.set_Minimum(new decimal(new int[4] { 1, 0, 0, 0 }));
			((Control)updnDAteRangeInAsteroidASelection).set_Name("updnDAteRangeInAsteroidASelection");
			((Control)updnDAteRangeInAsteroidASelection).set_Size(new Size(42, 20));
			((Control)updnDAteRangeInAsteroidASelection).set_TabIndex(5);
			((UpDownBase)updnDAteRangeInAsteroidASelection).set_TextAlign((HorizontalAlignment)2);
			updnDAteRangeInAsteroidASelection.set_Value(Settings.Default.DateRangeInAsteroidDisplaySelectionCriterion);
			((Control)chkExportElements).set_AutoSize(true);
			chkExportElements.set_Checked(Settings.Default.AsteroidExportEnabled);
			((Control)chkExportElements).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidExportEnabled", true, (DataSourceUpdateMode)1));
			((Control)chkExportElements).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkExportElements).set_ForeColor(Color.DimGray);
			((Control)chkExportElements).set_Location(new Point(9, 21));
			((Control)chkExportElements).set_Name("chkExportElements");
			((Control)chkExportElements).set_Size(new Size(546, 17));
			((Control)chkExportElements).set_TabIndex(7);
			((Control)chkExportElements).set_Text("When viewing predictions, you can export elements to a separate file. Check here to enable the export buttons");
			((ButtonBase)chkExportElements).set_UseVisualStyleBackColor(true);
			((Control)updnMaximumSearchEvents).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MaximumSearchEvents", true, (DataSourceUpdateMode)1));
			((Control)updnMaximumSearchEvents).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMaximumSearchEvents.set_Increment(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnMaximumSearchEvents).set_Location(new Point(312, 80));
			updnMaximumSearchEvents.set_Maximum(new decimal(new int[4] { 100000, 0, 0, 0 }));
			updnMaximumSearchEvents.set_Minimum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnMaximumSearchEvents).set_Name("updnMaximumSearchEvents");
			((Control)updnMaximumSearchEvents).set_Size(new Size(59, 20));
			((Control)updnMaximumSearchEvents).set_TabIndex(67);
			updnMaximumSearchEvents.set_Value(Settings.Default.MaximumSearchEvents);
			updnMaximumSearchEvents.add_ValueChanged((EventHandler)updnMaximumSearchEvents_ValueChanged);
			((Control)label182).set_AutoSize(true);
			((Control)label182).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label182).set_Location(new Point(11, 78));
			((Control)label182).set_Name("label182");
			((Control)label182).set_Size(new Size(299, 52));
			((Control)label182).set_TabIndex(66);
			((Control)label182).set_Text(componentResourceManager.GetString("label182.Text"));
			((Control)chkAddSAO).set_AutoSize(true);
			chkAddSAO.set_Checked(Settings.Default.DefaultPrepointAddSAO);
			((Control)chkAddSAO).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "DefaultPrepointAddSAO", true, (DataSourceUpdateMode)1));
			((Control)chkAddSAO).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkAddSAO).set_Location(new Point(465, 403));
			((Control)chkAddSAO).set_Name("chkAddSAO");
			((Control)chkAddSAO).set_Size(new Size(168, 17));
			((Control)chkAddSAO).set_TabIndex(64);
			((Control)chkAddSAO).set_Text("Add SAO #'s to Pre-point stars");
			((ButtonBase)chkAddSAO).set_UseVisualStyleBackColor(true);
			((Control)checkBox6).set_AutoSize(true);
			checkBox6.set_Checked(Settings.Default.AutoAsteroidDoubleInfo);
			checkBox6.set_CheckState((CheckState)1);
			((Control)checkBox6).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidDoubleInfo", true, (DataSourceUpdateMode)1));
			((Control)checkBox6).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox6).set_Location(new Point(465, 359));
			((Control)checkBox6).set_Name("checkBox6");
			((Control)checkBox6).set_Size(new Size(100, 17));
			((Control)checkBox6).set_TabIndex(61);
			((Control)checkBox6).set_Text("Double star info");
			((ButtonBase)checkBox6).set_UseVisualStyleBackColor(true);
			((Control)label113).set_AutoSize(true);
			((Control)label113).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label113).set_Location(new Point(462, 426));
			((Control)label113).set_Name("label113");
			((Control)label113).set_Size(new Size(227, 26));
			((Control)label113).set_TabIndex(60);
			((Control)label113).set_Text("Settings for star chart - see Star Chart start-up, \r\nand AutoGenerate (above)\r\n");
			cmbAutoFileType.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbAutoFileType).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbAutoFileType).set_FormattingEnabled(true);
			cmbAutoFileType.get_Items().AddRange(new object[5] { "JPEG", "BMP", "GIF", "PNG", "TIFF" });
			((Control)cmbAutoFileType).set_Location(new Point(269, 431));
			((Control)cmbAutoFileType).set_Name("cmbAutoFileType");
			((Control)cmbAutoFileType).set_Size(new Size(65, 21));
			((Control)cmbAutoFileType).set_TabIndex(59);
			cmbAutoFileType.add_SelectedIndexChanged((EventHandler)cmbAutoFileType_SelectedIndexChanged);
			((Control)label108).set_AutoSize(true);
			((Control)label108).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label108).set_Location(new Point(75, 434));
			((Control)label108).set_Name("label108");
			((Control)label108).set_Size(new Size(192, 13));
			((Control)label108).set_TabIndex(58);
			((Control)label108).set_Text("File format for World map and star chart");
			((Control)chkBessel).set_AutoSize(true);
			chkBessel.set_Checked(Settings.Default.AutoAsteroidBessel);
			chkBessel.set_CheckState((CheckState)1);
			((Control)chkBessel).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidBessel", true, (DataSourceUpdateMode)1));
			((Control)chkBessel).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkBessel).set_Location(new Point(572, 375));
			((Control)chkBessel).set_Name("chkBessel");
			((Control)chkBessel).set_Size(new Size(116, 17));
			((Control)chkBessel).set_TabIndex(57);
			((Control)chkBessel).set_Text("Besselian elements");
			((ButtonBase)chkBessel).set_UseVisualStyleBackColor(true);
			((Control)chkColour).set_AutoSize(true);
			chkColour.set_Checked(Settings.Default.AutoAsteroidColour);
			((Control)chkColour).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidColour", true, (DataSourceUpdateMode)1));
			((Control)chkColour).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkColour).set_Location(new Point(270, 359));
			((Control)chkColour).set_Name("chkColour");
			((Control)chkColour).set_Size(new Size(76, 17));
			((Control)chkColour).set_TabIndex(48);
			((Control)chkColour).set_Text("...in Colour");
			((ButtonBase)chkColour).set_UseVisualStyleBackColor(true);
			((Control)label84).set_AutoSize(true);
			((Control)label84).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label84).set_Location(new Point(13, 343));
			((Control)label84).set_Name("label84");
			((Control)label84).set_Size(new Size(160, 13));
			((Control)label84).set_TabIndex(45);
			((Control)label84).set_Text("Autogeneration output files");
			cmbSiteFiles.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbSiteFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(269, 403));
			cmbSiteFiles.set_MaxDropDownItems(20);
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(141, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(56);
			cmbSiteFiles.add_SelectedIndexChanged((EventHandler)cmbSiteFiles_SelectedIndexChanged);
			((Control)updnTitanDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Radius_Titan", true, (DataSourceUpdateMode)1));
			((Control)updnTitanDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnTitanDiameter).set_Location(new Point(655, 278));
			updnTitanDiameter.set_Maximum(new decimal(new int[4] { 2650, 0, 0, 0 }));
			updnTitanDiameter.set_Minimum(new decimal(new int[4] { 2500, 0, 0, 0 }));
			((Control)updnTitanDiameter).set_Name("updnTitanDiameter");
			((Control)updnTitanDiameter).set_Size(new Size(61, 20));
			((Control)updnTitanDiameter).set_TabIndex(42);
			((UpDownBase)updnTitanDiameter).set_TextAlign((HorizontalAlignment)2);
			updnTitanDiameter.set_Value(Settings.Default.Radius_Titan);
			((Control)label41).set_AutoSize(true);
			((Control)label41).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label41).set_Location(new Point(104, 405));
			((Control)label41).set_Name("label41");
			((Control)label41).set_Size(new Size(163, 13));
			((Control)label41).set_TabIndex(55);
			((Control)label41).set_Text("Site file to use in World map plots");
			((Control)chkHTM).set_AutoSize(true);
			chkHTM.set_Checked(Settings.Default.AutoAsteroidHTM);
			chkHTM.set_CheckState((CheckState)1);
			((Control)chkHTM).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidHTM", true, (DataSourceUpdateMode)1));
			((Control)chkHTM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkHTM).set_Location(new Point(572, 343));
			((Control)chkHTM).set_Name("chkHTM");
			((Control)chkHTM).set_Size(new Size(113, 17));
			((Control)chkHTM).set_TabIndex(53);
			((Control)chkHTM).set_Text("GoogleMaps HTM");
			((ButtonBase)chkHTM).set_UseVisualStyleBackColor(true);
			chkHTM.add_CheckedChanged((EventHandler)chkHTM_CheckedChanged);
			((Control)updnTritonDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Radius_Triton", true, (DataSourceUpdateMode)1));
			((Control)updnTritonDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnTritonDiameter).set_Location(new Point(655, 304));
			updnTritonDiameter.set_Maximum(new decimal(new int[4] { 1400, 0, 0, 0 }));
			updnTritonDiameter.set_Minimum(new decimal(new int[4] { 1300, 0, 0, 0 }));
			((Control)updnTritonDiameter).set_Name("updnTritonDiameter");
			((Control)updnTritonDiameter).set_Size(new Size(61, 20));
			((Control)updnTritonDiameter).set_TabIndex(44);
			((UpDownBase)updnTritonDiameter).set_TextAlign((HorizontalAlignment)2);
			updnTritonDiameter.set_Value(Settings.Default.Radius_Triton);
			((Control)chkGoogleEarth).set_AutoSize(true);
			chkGoogleEarth.set_Checked(Settings.Default.AutoAsteroidKML);
			chkGoogleEarth.set_CheckState((CheckState)1);
			((Control)chkGoogleEarth).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidKML", true, (DataSourceUpdateMode)1));
			((Control)chkGoogleEarth).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkGoogleEarth).set_Location(new Point(572, 359));
			((Control)chkGoogleEarth).set_Name("chkGoogleEarth");
			((Control)chkGoogleEarth).set_Size(new Size(111, 17));
			((Control)chkGoogleEarth).set_TabIndex(54);
			((Control)chkGoogleEarth).set_Text("GoogleEarth KMZ");
			((ButtonBase)chkGoogleEarth).set_UseVisualStyleBackColor(true);
			chkGoogleEarth.add_CheckedChanged((EventHandler)chkGoogleEarth_CheckedChanged);
			((Control)label78).set_AutoSize(true);
			((Control)label78).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label78).set_Location(new Point(273, 41));
			((Control)label78).set_Name("label78");
			((Control)label78).set_Size(new Size(405, 26));
			((Control)label78).set_TabIndex(3);
			((Control)label78).set_Text("NOTE:  This is only used if the source of elements is MPCORB or AstDyS2,\r\nand ASTORB is not present. It provides the uncertainty for each asteroid individually.");
			((Control)chkPrepoint).set_AutoSize(true);
			chkPrepoint.set_Checked(Settings.Default.AutoAsteroidPrePoint);
			chkPrepoint.set_CheckState((CheckState)1);
			((Control)chkPrepoint).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidPrePoint", true, (DataSourceUpdateMode)1));
			((Control)chkPrepoint).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPrepoint).set_Location(new Point(465, 375));
			((Control)chkPrepoint).set_Name("chkPrepoint");
			((Control)chkPrepoint).set_Size(new Size(93, 17));
			((Control)chkPrepoint).set_TabIndex(51);
			((Control)chkPrepoint).set_Text("Pre-point stars");
			((ButtonBase)chkPrepoint).set_UseVisualStyleBackColor(true);
			((Control)updnPlutoDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Radius_Pluto", true, (DataSourceUpdateMode)1));
			((Control)updnPlutoDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnPlutoDiameter).set_Location(new Point(526, 278));
			updnPlutoDiameter.set_Maximum(new decimal(new int[4] { 1300, 0, 0, 0 }));
			updnPlutoDiameter.set_Minimum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			((Control)updnPlutoDiameter).set_Name("updnPlutoDiameter");
			((Control)updnPlutoDiameter).set_Size(new Size(55, 20));
			((Control)updnPlutoDiameter).set_TabIndex(40);
			((UpDownBase)updnPlutoDiameter).set_TextAlign((HorizontalAlignment)2);
			updnPlutoDiameter.set_Value(Settings.Default.Radius_Pluto);
			((Control)chkMultisite).set_AutoSize(true);
			chkMultisite.set_Checked(Settings.Default.AutoAsteroidMultisite);
			chkMultisite.set_CheckState((CheckState)1);
			((Control)chkMultisite).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidMultisite", true, (DataSourceUpdateMode)1));
			((Control)chkMultisite).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMultisite).set_Location(new Point(353, 359));
			((Control)chkMultisite).set_Name("chkMultisite");
			((Control)chkMultisite).set_Size(new Size(79, 17));
			((Control)chkMultisite).set_TabIndex(50);
			((Control)chkMultisite).set_Text("Station sort");
			((ButtonBase)chkMultisite).set_UseVisualStyleBackColor(true);
			((Control)label77).set_AutoSize(true);
			((Control)label77).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label77).set_Location(new Point(9, 19));
			((Control)label77).set_Name("label77");
			((Control)label77).set_Size(new Size(432, 13));
			((Control)label77).set_TabIndex(0);
			((Control)label77).set_Text("These settings are used in the search and display of occultations by asteroids and planets.");
			((Control)chkPathCoords).set_AutoSize(true);
			chkPathCoords.set_Checked(Settings.Default.AutoAsteroidPathCoords);
			chkPathCoords.set_CheckState((CheckState)1);
			((Control)chkPathCoords).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidPathCoords", true, (DataSourceUpdateMode)1));
			((Control)chkPathCoords).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkPathCoords).set_Location(new Point(353, 343));
			((Control)chkPathCoords).set_Name("chkPathCoords");
			((Control)chkPathCoords).set_Size(new Size(106, 17));
			((Control)chkPathCoords).set_TabIndex(49);
			((Control)chkPathCoords).set_Text("Path coordinates");
			((ButtonBase)chkPathCoords).set_UseVisualStyleBackColor(true);
			chkPathCoords.add_CheckedChanged((EventHandler)chkPathCoords_CheckedChanged);
			((Control)chkStarChart).set_AutoSize(true);
			chkStarChart.set_Checked(Settings.Default.AutoAsteroidStarChart);
			chkStarChart.set_CheckState((CheckState)1);
			((Control)chkStarChart).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidStarChart", true, (DataSourceUpdateMode)1));
			((Control)chkStarChart).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkStarChart).set_Location(new Point(465, 343));
			((Control)chkStarChart).set_Name("chkStarChart");
			((Control)chkStarChart).set_Size(new Size(72, 17));
			((Control)chkStarChart).set_TabIndex(52);
			((Control)chkStarChart).set_Text("Star chart");
			((ButtonBase)chkStarChart).set_UseVisualStyleBackColor(true);
			((Control)label63).set_AutoSize(true);
			((Control)label63).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label63).set_Location(new Point(620, 308));
			((Control)label63).set_Name("label63");
			((Control)label63).set_Size(new Size(34, 13));
			((Control)label63).set_TabIndex(43);
			((Control)label63).set_Text("Triton");
			((Control)chkWorld).set_AutoSize(true);
			chkWorld.set_Checked(Settings.Default.AutoAsteroidWorld);
			chkWorld.set_CheckState((CheckState)1);
			((Control)chkWorld).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AutoAsteroidWorld", true, (DataSourceUpdateMode)1));
			((Control)chkWorld).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkWorld).set_Location(new Point(270, 343));
			((Control)chkWorld).set_Name("chkWorld");
			((Control)chkWorld).set_Size(new Size(77, 17));
			((Control)chkWorld).set_TabIndex(47);
			((Control)chkWorld).set_Text("World map");
			((ButtonBase)chkWorld).set_UseVisualStyleBackColor(true);
			((Control)updnUranusDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Radius_Uranus", true, (DataSourceUpdateMode)1));
			((Control)updnUranusDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnUranusDiameter).set_Location(new Point(397, 278));
			updnUranusDiameter.set_Maximum(new decimal(new int[4] { 26000, 0, 0, 0 }));
			updnUranusDiameter.set_Minimum(new decimal(new int[4] { 25000, 0, 0, 0 }));
			((Control)updnUranusDiameter).set_Name("updnUranusDiameter");
			((Control)updnUranusDiameter).set_Size(new Size(55, 20));
			((Control)updnUranusDiameter).set_TabIndex(36);
			((UpDownBase)updnUranusDiameter).set_TextAlign((HorizontalAlignment)2);
			updnUranusDiameter.set_Value(Settings.Default.Radius_Uranus);
			((Control)chkAutoGenerate).set_AutoSize(true);
			chkAutoGenerate.set_Checked(Settings.Default.AsteroidalsAutoPredictFutureOutputs);
			((Control)chkAutoGenerate).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "AsteroidalsAutoPredictFutureOutputs", true, (DataSourceUpdateMode)1));
			((Control)chkAutoGenerate).set_Enabled(false);
			((Control)chkAutoGenerate).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Strikeout, GraphicsUnit.Point, 0));
			((Control)chkAutoGenerate).set_Location(new Point(13, 361));
			((Control)chkAutoGenerate).set_Name("chkAutoGenerate");
			((Control)chkAutoGenerate).set_Size(new Size(178, 17));
			((Control)chkAutoGenerate).set_TabIndex(46);
			((Control)chkAutoGenerate).set_Text("Enable generation of output files");
			((ButtonBase)chkAutoGenerate).set_UseVisualStyleBackColor(true);
			((Control)label61).set_AutoSize(true);
			((Control)label61).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label61).set_Location(new Point(623, 282));
			((Control)label61).set_Name("label61");
			((Control)label61).set_Size(new Size(31, 13));
			((Control)label61).set_TabIndex(41);
			((Control)label61).set_Text("Titan");
			((Control)updnJupiterDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Radius_Jupiter", true, (DataSourceUpdateMode)1));
			((Control)updnJupiterDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnJupiterDiameter).set_Location(new Point(268, 278));
			updnJupiterDiameter.set_Maximum(new decimal(new int[4] { 73000, 0, 0, 0 }));
			updnJupiterDiameter.set_Minimum(new decimal(new int[4] { 70000, 0, 0, 0 }));
			((Control)updnJupiterDiameter).set_Name("updnJupiterDiameter");
			((Control)updnJupiterDiameter).set_Size(new Size(55, 20));
			((Control)updnJupiterDiameter).set_TabIndex(32);
			((UpDownBase)updnJupiterDiameter).set_TextAlign((HorizontalAlignment)2);
			updnJupiterDiameter.set_Value(Settings.Default.Radius_Jupiter);
			((Control)updnSaturnDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Radius_Saturn", true, (DataSourceUpdateMode)1));
			((Control)updnSaturnDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnSaturnDiameter).set_Location(new Point(268, 304));
			updnSaturnDiameter.set_Maximum(new decimal(new int[4] { 62000, 0, 0, 0 }));
			updnSaturnDiameter.set_Minimum(new decimal(new int[4] { 59000, 0, 0, 0 }));
			((Control)updnSaturnDiameter).set_Name("updnSaturnDiameter");
			((Control)updnSaturnDiameter).set_Size(new Size(55, 20));
			((Control)updnSaturnDiameter).set_TabIndex(34);
			((UpDownBase)updnSaturnDiameter).set_TextAlign((HorizontalAlignment)2);
			updnSaturnDiameter.set_Value(Settings.Default.Radius_Saturn);
			((Control)label62).set_AutoSize(true);
			((Control)label62).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label62).set_Location(new Point(493, 282));
			((Control)label62).set_Name("label62");
			((Control)label62).set_Size(new Size(31, 13));
			((Control)label62).set_TabIndex(39);
			((Control)label62).set_Text("Pluto");
			((Control)updnUncertainty).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "AsteroidSearchDefaultUncertainty", true, (DataSourceUpdateMode)1));
			updnUncertainty.set_DecimalPlaces(2);
			((Control)updnUncertainty).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnUncertainty.set_Increment(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnUncertainty).set_Location(new Point(222, 47));
			updnUncertainty.set_Maximum(new decimal(new int[4] { 1, 0, 0, 0 }));
			updnUncertainty.set_Minimum(new decimal(new int[4] { 5, 0, 0, 131072 }));
			((Control)updnUncertainty).set_Name("updnUncertainty");
			((Control)updnUncertainty).set_Size(new Size(45, 20));
			((Control)updnUncertainty).set_TabIndex(2);
			updnUncertainty.set_Value(Settings.Default.AsteroidSearchDefaultUncertainty);
			((Control)label83).set_AutoSize(true);
			((Control)label83).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label83).set_Location(new Point(13, 281));
			((Control)label83).set_Name("label83");
			((Control)label83).set_Size(new Size(209, 39));
			((Control)label83).set_TabIndex(30);
			((Control)label83).set_Text("Adjust here the equatorial RADIUS (km) of \r\nobjects with atmospheres, to allow for a\r\nlarger effective radius \r\n");
			((Control)updnNeptuneDiameter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Radius_Neptune", true, (DataSourceUpdateMode)1));
			((Control)updnNeptuneDiameter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnNeptuneDiameter).set_Location(new Point(397, 304));
			updnNeptuneDiameter.set_Maximum(new decimal(new int[4] { 25000, 0, 0, 0 }));
			updnNeptuneDiameter.set_Minimum(new decimal(new int[4] { 24000, 0, 0, 0 }));
			((Control)updnNeptuneDiameter).set_Name("updnNeptuneDiameter");
			((Control)updnNeptuneDiameter).set_Size(new Size(55, 20));
			((Control)updnNeptuneDiameter).set_TabIndex(38);
			((UpDownBase)updnNeptuneDiameter).set_TextAlign((HorizontalAlignment)2);
			updnNeptuneDiameter.set_Value(Settings.Default.Radius_Neptune);
			((Control)updnUranus).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitUranus", true, (DataSourceUpdateMode)1));
			updnUranus.set_DecimalPlaces(1);
			((Control)updnUranus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnUranus.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnUranus).set_Location(new Point(526, 240));
			updnUranus.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnUranus.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnUranus).set_Name("updnUranus");
			((Control)updnUranus).set_Size(new Size(50, 20));
			((Control)updnUranus).set_TabIndex(25);
			((UpDownBase)updnUranus).set_TextAlign((HorizontalAlignment)2);
			updnUranus.set_Value(Settings.Default.MagLimitUranus);
			((Control)label60).set_AutoSize(true);
			((Control)label60).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label60).set_Location(new Point(347, 308));
			((Control)label60).set_Name("label60");
			((Control)label60).set_Size(new Size(48, 13));
			((Control)label60).set_TabIndex(37);
			((Control)label60).set_Text("Neptune");
			((Control)label27).set_AutoSize(true);
			((Control)label27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label27).set_Location(new Point(13, 220));
			((Control)label27).set_Name("label27");
			((Control)label27).set_Size(new Size(196, 39));
			((Control)label27).set_TabIndex(13);
			((Control)label27).set_Text("When searching for occultations by the \r\nmajor planets, set the limiting magnitude \r\nof the star");
			((Control)label59).set_AutoSize(true);
			((Control)label59).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label59).set_Location(new Point(354, 282));
			((Control)label59).set_Name("label59");
			((Control)label59).set_Size(new Size(41, 13));
			((Control)label59).set_TabIndex(35);
			((Control)label59).set_Text("Uranus");
			((Control)label28).set_AutoSize(true);
			((Control)label28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label28).set_Location(new Point(221, 221));
			((Control)label28).set_Name("label28");
			((Control)label28).set_Size(new Size(45, 13));
			((Control)label28).set_TabIndex(14);
			((Control)label28).set_Text("Mercury");
			((Control)label58).set_AutoSize(true);
			((Control)label58).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label58).set_Location(new Point(229, 308));
			((Control)label58).set_Name("label58");
			((Control)label58).set_Size(new Size(38, 13));
			((Control)label58).set_TabIndex(33);
			((Control)label58).set_Text("Saturn");
			((Control)updnMars).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitMars", true, (DataSourceUpdateMode)1));
			updnMars.set_DecimalPlaces(1);
			((Control)updnMars).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMars.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMars).set_Location(new Point(397, 218));
			updnMars.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnMars.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnMars).set_Name("updnMars");
			((Control)updnMars).set_Size(new Size(50, 20));
			((Control)updnMars).set_TabIndex(19);
			((UpDownBase)updnMars).set_TextAlign((HorizontalAlignment)2);
			updnMars.set_Value(Settings.Default.MagLimitMars);
			((Control)label57).set_AutoSize(true);
			((Control)label57).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label57).set_Location(new Point(229, 282));
			((Control)label57).set_Name("label57");
			((Control)label57).set_Size(new Size(38, 13));
			((Control)label57).set_TabIndex(31);
			((Control)label57).set_Text("Jupiter");
			((Control)label29).set_AutoSize(true);
			((Control)label29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label29).set_Location(new Point(229, 243));
			((Control)label29).set_Name("label29");
			((Control)label29).set_Size(new Size(37, 13));
			((Control)label29).set_TabIndex(16);
			((Control)label29).set_Text("Venus");
			((Control)updnJupiter).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitJupiter", true, (DataSourceUpdateMode)1));
			updnJupiter.set_DecimalPlaces(1);
			((Control)updnJupiter).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnJupiter.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnJupiter).set_Location(new Point(397, 240));
			updnJupiter.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnJupiter.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnJupiter).set_Name("updnJupiter");
			((Control)updnJupiter).set_Size(new Size(50, 20));
			((Control)updnJupiter).set_TabIndex(21);
			((UpDownBase)updnJupiter).set_TextAlign((HorizontalAlignment)2);
			updnJupiter.set_Value(Settings.Default.MagLimitJupiter);
			((Control)label30).set_AutoSize(true);
			((Control)label30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label30).set_Location(new Point(365, 221));
			((Control)label30).set_Name("label30");
			((Control)label30).set_Size(new Size(30, 13));
			((Control)label30).set_TabIndex(18);
			((Control)label30).set_Text("Mars");
			((Control)updnNeptune).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitNeptune", true, (DataSourceUpdateMode)1));
			updnNeptune.set_DecimalPlaces(1);
			((Control)updnNeptune).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnNeptune.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnNeptune).set_Location(new Point(655, 218));
			updnNeptune.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnNeptune.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnNeptune).set_Name("updnNeptune");
			((Control)updnNeptune).set_Size(new Size(50, 20));
			((Control)updnNeptune).set_TabIndex(27);
			((UpDownBase)updnNeptune).set_TextAlign((HorizontalAlignment)2);
			updnNeptune.set_Value(Settings.Default.MagLimitNeptune);
			((Control)label31).set_AutoSize(true);
			((Control)label31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label31).set_Location(new Point(357, 243));
			((Control)label31).set_Name("label31");
			((Control)label31).set_Size(new Size(38, 13));
			((Control)label31).set_TabIndex(20);
			((Control)label31).set_Text("Jupiter");
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(486, 221));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(38, 13));
			((Control)label32).set_TabIndex(22);
			((Control)label32).set_Text("Saturn");
			((Control)updnPluto).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitPluto", true, (DataSourceUpdateMode)1));
			updnPluto.set_DecimalPlaces(1);
			((Control)updnPluto).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnPluto.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnPluto).set_Location(new Point(655, 240));
			updnPluto.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnPluto.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnPluto).set_Name("updnPluto");
			((Control)updnPluto).set_Size(new Size(50, 20));
			((Control)updnPluto).set_TabIndex(29);
			((UpDownBase)updnPluto).set_TextAlign((HorizontalAlignment)2);
			updnPluto.set_Value(Settings.Default.MagLimitPluto);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(483, 244));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(41, 13));
			((Control)label33).set_TabIndex(24);
			((Control)label33).set_Text("Uranus");
			((Control)label34).set_AutoSize(true);
			((Control)label34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label34).set_Location(new Point(605, 221));
			((Control)label34).set_Name("label34");
			((Control)label34).set_Size(new Size(48, 13));
			((Control)label34).set_TabIndex(26);
			((Control)label34).set_Text("Neptune");
			((Control)updnSaturn).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitSaturn", true, (DataSourceUpdateMode)1));
			updnSaturn.set_DecimalPlaces(1);
			((Control)updnSaturn).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnSaturn.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnSaturn).set_Location(new Point(526, 218));
			updnSaturn.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnSaturn.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnSaturn).set_Name("updnSaturn");
			((Control)updnSaturn).set_Size(new Size(50, 20));
			((Control)updnSaturn).set_TabIndex(23);
			((UpDownBase)updnSaturn).set_TextAlign((HorizontalAlignment)2);
			updnSaturn.set_Value(Settings.Default.MagLimitSaturn);
			((Control)label35).set_AutoSize(true);
			((Control)label35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label35).set_Location(new Point(622, 243));
			((Control)label35).set_Name("label35");
			((Control)label35).set_Size(new Size(31, 13));
			((Control)label35).set_TabIndex(28);
			((Control)label35).set_Text("Pluto");
			((Control)updnVenus).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitVenus", true, (DataSourceUpdateMode)1));
			updnVenus.set_DecimalPlaces(1);
			((Control)updnVenus).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnVenus.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnVenus).set_Location(new Point(268, 240));
			updnVenus.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnVenus.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnVenus).set_Name("updnVenus");
			((Control)updnVenus).set_Size(new Size(50, 20));
			((Control)updnVenus).set_TabIndex(17);
			((UpDownBase)updnVenus).set_TextAlign((HorizontalAlignment)2);
			updnVenus.set_Value(Settings.Default.MagLimitVenus);
			((Control)updnMercury).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "MagLimitMercury", true, (DataSourceUpdateMode)1));
			updnMercury.set_DecimalPlaces(1);
			((Control)updnMercury).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnMercury.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMercury).set_Location(new Point(268, 218));
			updnMercury.set_Maximum(new decimal(new int[4] { 20, 0, 0, 0 }));
			updnMercury.set_Minimum(new decimal(new int[4] { 4, 0, 0, 0 }));
			((Control)updnMercury).set_Name("updnMercury");
			((Control)updnMercury).set_Size(new Size(50, 20));
			((Control)updnMercury).set_TabIndex(15);
			((UpDownBase)updnMercury).set_TextAlign((HorizontalAlignment)2);
			updnMercury.set_Value(Settings.Default.MagLimitMercury);
			((Control)grpGoogleEarth).set_BackColor(Color.AntiqueWhite);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)txtGoogleMapsAPI);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)label133);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)numericUpDown59);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)numericUpDown58);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)label159);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)label157);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)label75);
			((Control)grpGoogleEarth).get_Controls().Add((Control)(object)checkBox1);
			((Control)grpGoogleEarth).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpGoogleEarth).set_Location(new Point(13, 1294));
			((Control)grpGoogleEarth).set_Name("grpGoogleEarth");
			((Control)grpGoogleEarth).set_Size(new Size(758, 132));
			((Control)grpGoogleEarth).set_TabIndex(4);
			grpGoogleEarth.set_TabStop(false);
			((Control)grpGoogleEarth).set_Text("6. GoogleEarth  &&  GoogleMaps");
			((Control)txtGoogleMapsAPI).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GoogleMaps_API_key", true, (DataSourceUpdateMode)1));
			((Control)txtGoogleMapsAPI).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtGoogleMapsAPI).set_Location(new Point(279, 103));
			((Control)txtGoogleMapsAPI).set_Name("txtGoogleMapsAPI");
			((Control)txtGoogleMapsAPI).set_Size(new Size(303, 20));
			((Control)txtGoogleMapsAPI).set_TabIndex(7);
			((Control)txtGoogleMapsAPI).set_Text(Settings.Default.GoogleMaps_API_key);
			((Control)label133).set_AutoSize(true);
			((Control)label133).set_Font(new Font("Microsoft Sans Serif", 8.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label133).set_Location(new Point(35, 106));
			((Control)label133).set_Name("label133");
			((Control)label133).set_Size(new Size(229, 15));
			((Control)label133).set_TabIndex(6);
			((Control)label133).set_Text("Google Maps API-Key for map elevations");
			((Control)numericUpDown59).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GoogleMapsLineWidth", true, (DataSourceUpdateMode)1));
			numericUpDown59.set_DecimalPlaces(1);
			((Control)numericUpDown59).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			numericUpDown59.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)numericUpDown59).set_Location(new Point(554, 70));
			numericUpDown59.set_Maximum(new decimal(new int[4] { 9, 0, 0, 0 }));
			numericUpDown59.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)numericUpDown59).set_Name("numericUpDown59");
			((Control)numericUpDown59).set_Size(new Size(44, 20));
			((Control)numericUpDown59).set_TabIndex(5);
			numericUpDown59.set_Value(Settings.Default.GoogleMapsLineWidth);
			((Control)numericUpDown58).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GoogleEarthLineWidth", true, (DataSourceUpdateMode)1));
			numericUpDown58.set_DecimalPlaces(1);
			((Control)numericUpDown58).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			numericUpDown58.set_Increment(new decimal(new int[4] { 1, 0, 0, 65536 }));
			((Control)numericUpDown58).set_Location(new Point(267, 70));
			numericUpDown58.set_Maximum(new decimal(new int[4] { 9, 0, 0, 0 }));
			numericUpDown58.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)numericUpDown58).set_Name("numericUpDown58");
			((Control)numericUpDown58).set_Size(new Size(44, 20));
			((Control)numericUpDown58).set_TabIndex(4);
			numericUpDown58.set_Value(Settings.Default.GoogleEarthLineWidth);
			((Control)label159).set_AutoSize(true);
			((Control)label159).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label159).set_Location(new Point(377, 73));
			((Control)label159).set_Name("label159");
			((Control)label159).set_Size(new Size(176, 13));
			((Control)label159).set_TabIndex(3);
			((Control)label159).set_Text("- drawn in Google Maps HTML files:");
			((Control)label157).set_AutoSize(true);
			((Control)label157).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label157).set_Location(new Point(14, 73));
			((Control)label157).set_Name("label157");
			((Control)label157).set_Size(new Size(252, 13));
			((Control)label157).set_TabIndex(2);
			((Control)label157).set_Text("Line width for lines drawn in Google Earth KMZ files:");
			((Control)label75).set_AutoSize(true);
			((Control)label75).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label75).set_Location(new Point(15, 22));
			((Control)label75).set_Name("label75");
			((Control)label75).set_Size(new Size(609, 13));
			((Control)label75).set_TabIndex(0);
			((Control)label75).set_Text("Occult can draw many maps directly onto GoogleEarth.   Indicate here whether GoogleEarth has been installed on this computer");
			((Control)checkBox1).set_AutoSize(true);
			checkBox1.set_Checked(Settings.Default.GoogleEarthInstalled);
			checkBox1.set_CheckState((CheckState)1);
			((Control)checkBox1).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "GoogleEarthInstalled", true, (DataSourceUpdateMode)1));
			((Control)checkBox1).set_Font(new Font("Microsoft Sans Serif", 8.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)checkBox1).set_Location(new Point(16, 39));
			((Control)checkBox1).set_Name("checkBox1");
			((Control)checkBox1).set_Size(new Size(250, 19));
			((Control)checkBox1).set_TabIndex(1);
			((Control)checkBox1).set_Text("GoogleEarth is installed on this computer");
			((ButtonBase)checkBox1).set_UseVisualStyleBackColor(true);
			((Control)grpOptionalCatalogues).set_BackColor(Color.AntiqueWhite);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)cmdDownload_u4i_files);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label184);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label174);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)pictureBox2);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)cmdU4XRefs);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)cmdUCAC4CheckCorrected);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)pictureBox1);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)cmdU4Help);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label162);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)cmdFolder_UCAC4);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label160);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)txtUCAC4);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label141);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)cmdFolderNOMAD);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)cmdFolderNomad_Short);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label76);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label66);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)txtNomadShort);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)label64);
			((Control)grpOptionalCatalogues).get_Controls().Add((Control)(object)txtNOMAD);
			((Control)grpOptionalCatalogues).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpOptionalCatalogues).set_Location(new Point(13, 1451));
			((Control)grpOptionalCatalogues).set_Name("grpOptionalCatalogues");
			((Control)grpOptionalCatalogues).set_Size(new Size(758, 338));
			((Control)grpOptionalCatalogues).set_TabIndex(5);
			grpOptionalCatalogues.set_TabStop(false);
			((Control)grpOptionalCatalogues).set_Text("7. Paths for optional catalogues");
			((Control)cmdDownload_u4i_files).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdDownload_u4i_files).set_Location(new Point(563, 156));
			((Control)cmdDownload_u4i_files).set_Name("cmdDownload_u4i_files");
			((Control)cmdDownload_u4i_files).set_Size(new Size(139, 52));
			((Control)cmdDownload_u4i_files).set_TabIndex(41);
			((Control)cmdDownload_u4i_files).set_Text("Download UCAC4 supplementary files");
			((ButtonBase)cmdDownload_u4i_files).set_UseVisualStyleBackColor(true);
			((Control)cmdDownload_u4i_files).add_Click((EventHandler)cmdDownload_u4i_files_Click);
			((Control)label184).set_AutoSize(true);
			((Control)label184).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label184).set_Location(new Point(6, 156));
			((Control)label184).set_Name("label184");
			((Control)label184).set_Size(new Size(539, 52));
			((Control)label184).set_TabIndex(40);
			((Control)label184).set_Text(componentResourceManager.GetString("label184.Text"));
			((Control)label174).set_AutoSize(true);
			((Control)label174).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label174).set_Location(new Point(379, 222));
			((Control)label174).set_Name("label174");
			((Control)label174).set_Size(new Size(193, 12));
			((Control)label174).set_TabIndex(33);
			((Control)label174).set_Text("Select directory with uncompressed zone files");
			pictureBox2.set_Image((Image)Resources.help);
			((Control)pictureBox2).set_Location(new Point(388, 242));
			((Control)pictureBox2).set_Name("pictureBox2");
			((Control)pictureBox2).set_Size(new Size(19, 21));
			pictureBox2.set_SizeMode((PictureBoxSizeMode)1);
			pictureBox2.set_TabIndex(32);
			pictureBox2.set_TabStop(false);
			((Control)cmdU4XRefs).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdU4XRefs).set_Location(new Point(378, 122));
			((Control)cmdU4XRefs).set_Name("cmdU4XRefs");
			((Control)cmdU4XRefs).set_Size(new Size(100, 21));
			((Control)cmdU4XRefs).set_TabIndex(23);
			((Control)cmdU4XRefs).set_Text("Create xRef files");
			((ButtonBase)cmdU4XRefs).set_UseVisualStyleBackColor(true);
			((Control)cmdU4XRefs).add_Click((EventHandler)cmdU4HipLookup_Click);
			((Control)cmdUCAC4CheckCorrected).set_Font(new Font("Microsoft Sans Serif", 7.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdUCAC4CheckCorrected).set_Location(new Point(560, 122));
			((Control)cmdUCAC4CheckCorrected).set_Name("cmdUCAC4CheckCorrected");
			((Control)cmdUCAC4CheckCorrected).set_Size(new Size(103, 21));
			((Control)cmdUCAC4CheckCorrected).set_TabIndex(27);
			((Control)cmdUCAC4CheckCorrected).set_Text("Check if corrected");
			((ButtonBase)cmdUCAC4CheckCorrected).set_UseVisualStyleBackColor(true);
			((Control)cmdUCAC4CheckCorrected).add_Click((EventHandler)cmdUCAC4CheckCorrected_Click);
			pictureBox1.set_Image((Image)Resources.help);
			((Control)pictureBox1).set_Location(new Point(677, 122));
			((Control)pictureBox1).set_Name("pictureBox1");
			((Control)pictureBox1).set_Size(new Size(19, 21));
			pictureBox1.set_SizeMode((PictureBoxSizeMode)1);
			pictureBox1.set_TabIndex(26);
			pictureBox1.set_TabStop(false);
			((Control)cmdU4Help).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdU4Help).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdU4Help).set_Location(new Point(672, 119));
			((Control)cmdU4Help).set_Name("cmdU4Help");
			((Control)cmdU4Help).set_Size(new Size(79, 27));
			((Control)cmdU4Help).set_TabIndex(25);
			((Control)cmdU4Help).set_Text("U4 Help");
			((ButtonBase)cmdU4Help).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdU4Help).set_UseVisualStyleBackColor(true);
			((Control)cmdU4Help).add_Click((EventHandler)cmdU4Help_Click);
			((Control)label162).set_AutoSize(true);
			((Control)label162).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label162).set_Location(new Point(476, 120));
			((Control)label162).set_Name("label162");
			((Control)label162).set_Size(new Size(71, 24));
			((Control)label162).set_TabIndex(24);
			((Control)label162).set_Text("This will take\r\nseveral minutes");
			((Control)cmdFolder_UCAC4).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFolder_UCAC4).set_Location(new Point(335, 122));
			((Control)cmdFolder_UCAC4).set_Name("cmdFolder_UCAC4");
			((Control)cmdFolder_UCAC4).set_Size(new Size(29, 20));
			((Control)cmdFolder_UCAC4).set_TabIndex(18);
			((Control)cmdFolder_UCAC4).set_Text("set");
			((ButtonBase)cmdFolder_UCAC4).set_UseVisualStyleBackColor(true);
			((Control)cmdFolder_UCAC4).add_Click((EventHandler)cmdFolder_UCAC4_Click);
			((Control)label160).set_AutoSize(true);
			((Control)label160).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label160).set_Location(new Point(5, 126));
			((Control)label160).set_Name("label160");
			((Control)label160).set_Size(new Size(67, 13));
			((Control)label160).set_TabIndex(16);
			((Control)label160).set_Text("UCAC4 Path");
			((Control)txtUCAC4).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "UCAC4_Path", true, (DataSourceUpdateMode)1));
			((Control)txtUCAC4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtUCAC4).set_Location(new Point(74, 122));
			((Control)txtUCAC4).set_Name("txtUCAC4");
			((Control)txtUCAC4).set_Size(new Size(261, 20));
			((Control)txtUCAC4).set_TabIndex(17);
			((Control)txtUCAC4).set_Text(Settings.Default.UCAC4_Path);
			((Control)label141).set_AutoSize(true);
			((Control)label141).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Italic, GraphicsUnit.Point, 0));
			((Control)label141).set_Location(new Point(346, 94));
			((Control)label141).set_Name("label141");
			((Control)label141).set_Size(new Size(143, 24));
			((Control)label141).set_TabIndex(15);
			((Control)label141).set_Text("Allow between 20 and 60 minutes\r\ndepending on computer speed");
			((Control)cmdFolderNOMAD).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFolderNOMAD).set_Location(new Point(720, 67));
			((Control)cmdFolderNOMAD).set_Name("cmdFolderNOMAD");
			((Control)cmdFolderNOMAD).set_Size(new Size(29, 20));
			((Control)cmdFolderNOMAD).set_TabIndex(12);
			((Control)cmdFolderNOMAD).set_Text("set");
			((ButtonBase)cmdFolderNOMAD).set_UseVisualStyleBackColor(true);
			((Control)cmdFolderNOMAD).add_Click((EventHandler)cmdFolderNOMAD_Click);
			((Control)cmdFolderNomad_Short).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)cmdFolderNomad_Short).set_Location(new Point(720, 42));
			((Control)cmdFolderNomad_Short).set_Name("cmdFolderNomad_Short");
			((Control)cmdFolderNomad_Short).set_Size(new Size(29, 20));
			((Control)cmdFolderNomad_Short).set_TabIndex(6);
			((Control)cmdFolderNomad_Short).set_Text("set");
			((ButtonBase)cmdFolderNomad_Short).set_UseVisualStyleBackColor(true);
			((Control)cmdFolderNomad_Short).add_Click((EventHandler)cmdFolderNomad_Short_Click);
			((Control)label76).set_AutoSize(true);
			((Control)label76).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label76).set_Location(new Point(7, 22));
			((Control)label76).set_Name("label76");
			((Control)label76).set_Size(new Size(667, 13));
			((Control)label76).set_TabIndex(0);
			((Control)label76).set_Text("Occult can use the UCAC2 and NOMAD catalogues for asteroid searches, and star charts. To use thoe catalogues, indicate here their Path ");
			((Control)label66).set_AutoSize(true);
			((Control)label66).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label66).set_Location(new Point(377, 45));
			((Control)label66).set_Name("label66");
			((Control)label66).set_Size(new Size(79, 13));
			((Control)label66).set_TabIndex(4);
			((Control)label66).set_Text("NOMAD [short]");
			((Control)txtNomadShort).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "NOMAD_Short_path", true, (DataSourceUpdateMode)1));
			((Control)txtNomadShort).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtNomadShort).set_Location(new Point(457, 42));
			((Control)txtNomadShort).set_Name("txtNomadShort");
			((Control)txtNomadShort).set_Size(new Size(261, 20));
			((Control)txtNomadShort).set_TabIndex(5);
			((Control)txtNomadShort).set_Text(Settings.Default.NOMAD_Short_path);
			((Control)label64).set_AutoSize(true);
			((Control)label64).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label64).set_Location(new Point(386, 71));
			((Control)label64).set_Name("label64");
			((Control)label64).set_Size(new Size(69, 13));
			((Control)label64).set_TabIndex(10);
			((Control)label64).set_Text("NOMAD [full]");
			((Control)txtNOMAD).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "NOMAD_Path", true, (DataSourceUpdateMode)1));
			((Control)txtNOMAD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtNOMAD).set_Location(new Point(457, 67));
			((Control)txtNOMAD).set_Name("txtNOMAD");
			((Control)txtNOMAD).set_Size(new Size(261, 20));
			((Control)txtNOMAD).set_TabIndex(11);
			((Control)txtNOMAD).set_Text(Settings.Default.NOMAD_Path);
			toolTip1.SetToolTip((Control)(object)txtNOMAD, "Specify the Path for the 'Nomad1' directory");
			((Control)grpLunarOccultations).set_BackColor(Color.AntiqueWhite);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label172);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label171);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label170);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label169);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label168);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnMaxSep);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnMinSep);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)chkLunarCoordinates);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label129);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)chkMultiSiteAltitude);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)cmbLibrationsReduce);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label120);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label105);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label96);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label56);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label95);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label55);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label94);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label54);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label51);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label53);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)cmbGrazeProfile_FileType);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label52);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown37);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label50);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown38);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)cmbLibrations);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown39);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label49);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown40);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label45);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)textBox36);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label46);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown33);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnAlt);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown34);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnStep);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown35);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label47);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown36);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label48);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)textBox35);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnEndLong);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown29);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnStartLong);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown30);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)panel3);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown31);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)chkGrazeProfileKM_Mi);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown32);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label43);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)textBox34);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnGraze45);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown25);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)label42);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown26);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)updnGraze65);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown27);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)textBox32);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown28);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)textBox30);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)textBox33);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown13);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown21);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown14);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown22);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown15);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown23);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown16);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown24);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)textBox31);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown20);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown17);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown19);
			((Control)grpLunarOccultations).get_Controls().Add((Control)(object)numericUpDown18);
			((Control)grpLunarOccultations).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)grpLunarOccultations).set_Location(new Point(13, 3045));
			((Control)grpLunarOccultations).set_Name("grpLunarOccultations");
			((Control)grpLunarOccultations).set_Size(new Size(758, 566));
			((Control)grpLunarOccultations).set_TabIndex(10);
			grpLunarOccultations.set_TabStop(false);
			((Control)grpLunarOccultations).set_Text("13. Lunar Occultation Predictions -  Grazes, Multisite predictions");
			((Control)label172).set_AutoSize(true);
			((Control)label172).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label172).set_Location(new Point(618, 21));
			((Control)label172).set_Name("label172");
			((Control)label172).set_Size(new Size(132, 39));
			((Control)label172).set_TabIndex(74);
			((Control)label172).set_Text("[NOTE: Changes may only \r\ncome into effect after \r\nOccult is re-started.] ");
			((Control)label171).set_AutoSize(true);
			((Control)label171).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label171).set_Location(new Point(113, 34));
			((Control)label171).set_Name("label171");
			((Control)label171).set_Size(new Size(304, 13));
			((Control)label171).set_TabIndex(73);
			((Control)label171).set_Text("Flag that observations are desired, when separation is between");
			((Control)label170).set_AutoSize(true);
			((Control)label170).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label170).set_Location(new Point(576, 34));
			((Control)label170).set_Name("label170");
			((Control)label170).set_Size(new Size(39, 13));
			((Control)label170).set_TabIndex(72);
			((Control)label170).set_Text("arcsec");
			((Control)label169).set_AutoSize(true);
			((Control)label169).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label169).set_Location(new Point(469, 34));
			((Control)label169).set_Name("label169");
			((Control)label169).set_Size(new Size(60, 13));
			((Control)label169).set_TabIndex(71);
			((Control)label169).set_Text("arcsec and");
			((Control)label168).set_AutoSize(true);
			((Control)label168).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label168).set_Location(new Point(32, 34));
			((Control)label168).set_Name("label168");
			((Control)label168).set_Size(new Size(82, 13));
			((Control)label168).set_TabIndex(70);
			((Control)label168).set_Text("Double stars:");
			((Control)updnMaxSep).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "DoubleStarMaxSep", true, (DataSourceUpdateMode)1));
			updnMaxSep.set_DecimalPlaces(1);
			((Control)updnMaxSep).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMaxSep).set_Location(new Point(531, 33));
			updnMaxSep.set_Maximum(new decimal(new int[4] { 50, 0, 0, 0 }));
			updnMaxSep.set_Minimum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMaxSep).set_Name("updnMaxSep");
			((Control)updnMaxSep).set_Size(new Size(43, 20));
			((Control)updnMaxSep).set_TabIndex(69);
			updnMaxSep.set_Value(Settings.Default.DoubleStarMaxSep);
			((Control)updnMinSep).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "DoubleStarMinSep", true, (DataSourceUpdateMode)1));
			updnMinSep.set_DecimalPlaces(3);
			((Control)updnMinSep).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnMinSep).set_Location(new Point(420, 33));
			updnMinSep.set_Maximum(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnMinSep).set_Name("updnMinSep");
			((Control)updnMinSep).set_Size(new Size(47, 20));
			((Control)updnMinSep).set_TabIndex(68);
			updnMinSep.set_Value(Settings.Default.DoubleStarMinSep);
			((Control)chkLunarCoordinates).set_AutoSize(true);
			chkLunarCoordinates.set_Checked(Settings.Default.LunarPredict_DisplayApparentPosition);
			((Control)chkLunarCoordinates).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarPredict_DisplayApparentPosition", true, (DataSourceUpdateMode)1));
			((Control)chkLunarCoordinates).set_Font(new Font("Microsoft Sans Serif", 8f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkLunarCoordinates).set_Location(new Point(37, 61));
			((Control)chkLunarCoordinates).set_Name("chkLunarCoordinates");
			((Control)chkLunarCoordinates).set_Size(new Size(416, 17));
			((Control)chkLunarCoordinates).set_TabIndex(67);
			((Control)chkLunarCoordinates).set_Text("In lunar predictions, display Apparent star coordinates instead of J2000 coordinates");
			((ButtonBase)chkLunarCoordinates).set_UseVisualStyleBackColor(true);
			((Control)label129).set_AutoSize(true);
			((Control)label129).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label129).set_Location(new Point(32, 535));
			((Control)label129).set_Name("label129");
			((Control)label129).set_Size(new Size(139, 26));
			((Control)label129).set_TabIndex(66);
			((Control)label129).set_Text("Multi-site predictions of\r\na single star");
			label129.set_TextAlign(ContentAlignment.MiddleCenter);
			((Control)chkMultiSiteAltitude).set_AutoSize(true);
			chkMultiSiteAltitude.set_Checked(Settings.Default.LunarMultiSitePrediction_SunAlt);
			((Control)chkMultiSiteAltitude).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "LunarMultiSitePrediction_SunAlt", true, (DataSourceUpdateMode)1));
			((Control)chkMultiSiteAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkMultiSiteAltitude).set_Location(new Point(179, 540));
			((Control)chkMultiSiteAltitude).set_Name("chkMultiSiteAltitude");
			((Control)chkMultiSiteAltitude).set_Size(new Size(428, 17));
			((Control)chkMultiSiteAltitude).set_TabIndex(65);
			((Control)chkMultiSiteAltitude).set_Text("Exclude ALL events with sun altitude >-6 deg  ( that is, daytime events of bright stars )");
			((ButtonBase)chkMultiSiteAltitude).set_UseVisualStyleBackColor(true);
			cmbLibrationsReduce.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLibrationsReduce).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLibrationsReduce).set_FormattingEnabled(true);
			cmbLibrationsReduce.get_Items().AddRange(new object[10] { "None", "P, D[+/- 0.3 deg]", "P, D[+/- 0.5 deg]", "P, D[+/- 1.0 deg]", "AA, L[+/- 1 deg], B[+/- 0.5 deg]", "AA, L[+/- 2 deg], B[+/- 0.5 deg]", "AA, L[+/- 1 deg], B[+/- 1 deg]", "AA, L[+/- 2 deg], B[+/- 1 deg]", "AA, L[+/- 4 deg], B[+/- 1 deg]", "AA, L[+/- 4 deg], B[+/- 2 deg]" });
			((Control)cmbLibrationsReduce).set_Location(new Point(490, 328));
			cmbLibrationsReduce.set_MaxDropDownItems(10);
			((Control)cmbLibrationsReduce).set_Name("cmbLibrationsReduce");
			((Control)cmbLibrationsReduce).set_Size(new Size(176, 21));
			((Control)cmbLibrationsReduce).set_TabIndex(64);
			cmbLibrationsReduce.add_SelectedIndexChanged((EventHandler)cmbLibrationsReduce_SelectedIndexChanged);
			((Control)label120).set_AutoSize(true);
			((Control)label120).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label120).set_Location(new Point(383, 319));
			((Control)label120).set_Name("label120");
			((Control)label120).set_Size(new Size(104, 39));
			((Control)label120).set_TabIndex(63);
			((Control)label120).set_Text("Reduction profile: \r\nLibrations for plotting\r\nobserved data");
			label120.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)label105).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label105).set_Location(new Point(467, 98));
			((Control)label105).set_Name("label105");
			((Control)label105).set_Size(new Size(261, 53));
			((Control)label105).set_TabIndex(62);
			((Control)label105).set_Text("Occult can draw grazing occultation paths over any 'local' region of the Earth. The settings used for asteroid occultations also apply to graze maps.\r\n");
			((Control)label96).set_AutoSize(true);
			((Control)label96).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label96).set_Location(new Point(35, 372));
			((Control)label96).set_Name("label96");
			((Control)label96).set_Size(new Size(271, 13));
			((Control)label96).set_TabIndex(21);
			((Control)label96).set_Text("Occult can generate graze predictions of bright stars for ");
			((Control)label56).set_AutoSize(true);
			((Control)label56).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label56).set_Location(new Point(509, 373));
			((Control)label56).set_Name("label56");
			((Control)label56).set_Size(new Size(28, 13));
			((Control)label56).set_TabIndex(24);
			((Control)label56).set_Text("East");
			((Control)label95).set_AutoSize(true);
			((Control)label95).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label95).set_Location(new Point(150, 124));
			((Control)label95).set_Name("label95");
			((Control)label95).set_Size(new Size(76, 13));
			((Control)label95).set_TabIndex(2);
			((Control)label95).set_Text("stars <6.5 (km)");
			((Control)label55).set_AutoSize(true);
			((Control)label55).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label55).set_Location(new Point(580, 373));
			((Control)label55).set_Name("label55");
			((Control)label55).set_Size(new Size(33, 13));
			((Control)label55).set_TabIndex(25);
			((Control)label55).set_Text("North");
			((Control)label94).set_AutoSize(true);
			((Control)label94).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label94).set_Location(new Point(62, 83));
			((Control)label94).set_Name("label94");
			((Control)label94).set_Size(new Size(401, 13));
			((Control)label94).set_TabIndex(0);
			((Control)label94).set_Text("Default values for lunar occultations - mainly for grazing occultations.");
			((Control)label54).set_AutoSize(true);
			((Control)label54).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label54).set_Location(new Point(639, 373));
			((Control)label54).set_Name("label54");
			((Control)label54).set_Size(new Size(35, 13));
			((Control)label54).set_TabIndex(26);
			((Control)label54).set_Text("South");
			((Control)label51).set_AutoSize(true);
			((Control)label51).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label51).set_Location(new Point(33, 297));
			((Control)label51).set_Name("label51");
			((Control)label51).set_Size(new Size(130, 13));
			((Control)label51).set_TabIndex(17);
			((Control)label51).set_Text("File format for graze profile");
			((Control)label53).set_AutoSize(true);
			((Control)label53).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label53).set_Location(new Point(440, 373));
			((Control)label53).set_Name("label53");
			((Control)label53).set_Size(new Size(32, 13));
			((Control)label53).set_TabIndex(23);
			((Control)label53).set_Text("West");
			cmbGrazeProfile_FileType.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbGrazeProfile_FileType).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbGrazeProfile_FileType).set_FormattingEnabled(true);
			cmbGrazeProfile_FileType.get_Items().AddRange(new object[5] { "JPEG", "BMP", "GIF", "PNG", "TIFF" });
			((Control)cmbGrazeProfile_FileType).set_Location(new Point(165, 294));
			((Control)cmbGrazeProfile_FileType).set_Name("cmbGrazeProfile_FileType");
			((Control)cmbGrazeProfile_FileType).set_Size(new Size(65, 21));
			((Control)cmbGrazeProfile_FileType).set_TabIndex(18);
			cmbGrazeProfile_FileType.add_SelectedIndexChanged((EventHandler)cmbFileType_SelectedIndexChanged);
			((Control)label52).set_AutoSize(true);
			((Control)label52).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label52).set_Location(new Point(314, 373));
			((Control)label52).set_Name("label52");
			((Control)label52).set_Size(new Size(41, 13));
			((Control)label52).set_TabIndex(22);
			((Control)label52).set_Text("Region");
			((Control)numericUpDown37).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapS6", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown37).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown37).set_Location(new Point(640, 508));
			numericUpDown37.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown37.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown37).set_Name("numericUpDown37");
			((Control)numericUpDown37).set_Size(new Size(49, 20));
			((Control)numericUpDown37).set_TabIndex(61);
			((UpDownBase)numericUpDown37).set_TextAlign((HorizontalAlignment)1);
			numericUpDown37.set_Value(Settings.Default.GMapS6);
			((Control)label50).set_AutoSize(true);
			((Control)label50).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label50).set_Location(new Point(34, 319));
			((Control)label50).set_Name("label50");
			((Control)label50).set_Size(new Size(104, 39));
			((Control)label50).set_TabIndex(19);
			((Control)label50).set_Text("Prediction profile:\r\nLibrations for plotting\r\nobserved data");
			label50.set_TextAlign(ContentAlignment.MiddleRight);
			((Control)numericUpDown38).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapN6", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown38).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown38).set_Location(new Point(578, 508));
			numericUpDown38.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown38.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown38).set_Name("numericUpDown38");
			((Control)numericUpDown38).set_Size(new Size(49, 20));
			((Control)numericUpDown38).set_TabIndex(60);
			((UpDownBase)numericUpDown38).set_TextAlign((HorizontalAlignment)1);
			numericUpDown38.set_Value(Settings.Default.GMapN6);
			cmbLibrations.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbLibrations).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbLibrations).set_FormattingEnabled(true);
			cmbLibrations.get_Items().AddRange(new object[10] { "None", "P, D[+/- 0.3 deg]", "P, D[+/- 0.5 deg]", "P, D[+/- 1.0 deg]", "AA, L[+/- 1 deg], B[+/- 0.5 deg]", "AA, L[+/- 2 deg], B[+/- 0.5 deg]", "AA, L[+/- 1 deg], B[+/- 1 deg]", "AA, L[+/- 2 deg], B[+/- 1 deg]", "AA, L[+/- 4 deg], B[+/- 1 deg]", "AA, L[+/- 4 deg], B[+/- 2 deg]" });
			((Control)cmbLibrations).set_Location(new Point(140, 328));
			cmbLibrations.set_MaxDropDownItems(10);
			((Control)cmbLibrations).set_Name("cmbLibrations");
			((Control)cmbLibrations).set_Size(new Size(176, 21));
			((Control)cmbLibrations).set_TabIndex(20);
			cmbLibrations.add_SelectedIndexChanged((EventHandler)cmbLibrations_SelectedIndexChanged);
			((Control)numericUpDown39).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapE6", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown39).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown39).set_Location(new Point(503, 508));
			numericUpDown39.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown39.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown39).set_Name("numericUpDown39");
			((Control)numericUpDown39).set_Size(new Size(56, 20));
			((Control)numericUpDown39).set_TabIndex(59);
			((UpDownBase)numericUpDown39).set_TextAlign((HorizontalAlignment)1);
			numericUpDown39.set_Value(Settings.Default.GMapE6);
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(33, 188));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(98, 13));
			((Control)label49).set_TabIndex(6);
			((Control)label49).set_Text("Graze path settings");
			((Control)numericUpDown40).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapW6", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown40).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown40).set_Location(new Point(436, 508));
			numericUpDown40.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown40.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown40).set_Name("numericUpDown40");
			((Control)numericUpDown40).set_Size(new Size(56, 20));
			((Control)numericUpDown40).set_TabIndex(58);
			((UpDownBase)numericUpDown40).set_TextAlign((HorizontalAlignment)1);
			numericUpDown40.set_Value(Settings.Default.GMapW6);
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(330, 213));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(82, 13));
			((Control)label45).set_TabIndex(13);
			((Control)label45).set_Text("Nominal altitude");
			((Control)textBox36).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GMapName6", true, (DataSourceUpdateMode)1));
			((Control)textBox36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox36).set_Location(new Point(306, 508));
			((Control)textBox36).set_Name("textBox36");
			((Control)textBox36).set_Size(new Size(103, 20));
			((Control)textBox36).set_TabIndex(57);
			((Control)textBox36).set_Text(Settings.Default.GMapName6);
			((Control)textBox36).add_Leave((EventHandler)textBox36_Leave);
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(346, 190));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(66, 13));
			((Control)label46).set_TabIndex(11);
			((Control)label46).set_Text("Step interval");
			((Control)numericUpDown33).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapS5", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown33).set_Location(new Point(640, 488));
			numericUpDown33.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown33.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown33).set_Name("numericUpDown33");
			((Control)numericUpDown33).set_Size(new Size(49, 20));
			((Control)numericUpDown33).set_TabIndex(56);
			((UpDownBase)numericUpDown33).set_TextAlign((HorizontalAlignment)1);
			numericUpDown33.set_Value(Settings.Default.GMapS5);
			((Control)updnAlt).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Graze_DefaultAltitude", true, (DataSourceUpdateMode)1));
			((Control)updnAlt).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnAlt.set_Increment(new decimal(new int[4] { 50, 0, 0, 0 }));
			((Control)updnAlt).set_Location(new Point(418, 209));
			updnAlt.set_Maximum(new decimal(new int[4] { 10000, 0, 0, 0 }));
			updnAlt.set_Minimum(new decimal(new int[4] { 300, 0, 0, -2147483648 }));
			((Control)updnAlt).set_Name("updnAlt");
			((Control)updnAlt).set_Size(new Size(56, 20));
			((Control)updnAlt).set_TabIndex(14);
			updnAlt.set_Value(Settings.Default.Graze_DefaultAltitude);
			((Control)numericUpDown34).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapN5", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown34).set_Location(new Point(578, 488));
			numericUpDown34.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown34.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown34).set_Name("numericUpDown34");
			((Control)numericUpDown34).set_Size(new Size(49, 20));
			((Control)numericUpDown34).set_TabIndex(55);
			((UpDownBase)numericUpDown34).set_TextAlign((HorizontalAlignment)1);
			numericUpDown34.set_Value(Settings.Default.GMapN5);
			((Control)updnStep).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Graze_DefaultStep", true, (DataSourceUpdateMode)1));
			updnStep.set_DecimalPlaces(3);
			((Control)updnStep).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnStep.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnStep).set_Location(new Point(418, 186));
			updnStep.set_Maximum(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnStep.set_Minimum(new decimal(new int[4] { 1, 0, 0, 131072 }));
			((Control)updnStep).set_Name("updnStep");
			((Control)updnStep).set_Size(new Size(56, 20));
			((Control)updnStep).set_TabIndex(12);
			updnStep.set_Value(Settings.Default.Graze_DefaultStep);
			((Control)numericUpDown35).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapE5", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown35).set_Location(new Point(503, 488));
			numericUpDown35.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown35.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown35).set_Name("numericUpDown35");
			((Control)numericUpDown35).set_Size(new Size(56, 20));
			((Control)numericUpDown35).set_TabIndex(54);
			((UpDownBase)numericUpDown35).set_TextAlign((HorizontalAlignment)1);
			numericUpDown35.set_Value(Settings.Default.GMapE5);
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(154, 213));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(72, 13));
			((Control)label47).set_TabIndex(9);
			((Control)label47).set_Text("End longitude");
			((Control)numericUpDown36).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapW5", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown36).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown36).set_Location(new Point(436, 488));
			numericUpDown36.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown36.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown36).set_Name("numericUpDown36");
			((Control)numericUpDown36).set_Size(new Size(56, 20));
			((Control)numericUpDown36).set_TabIndex(53);
			((UpDownBase)numericUpDown36).set_TextAlign((HorizontalAlignment)1);
			numericUpDown36.set_Value(Settings.Default.GMapW5);
			((Control)label48).set_AutoSize(true);
			((Control)label48).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label48).set_Location(new Point(151, 190));
			((Control)label48).set_Name("label48");
			((Control)label48).set_Size(new Size(75, 13));
			((Control)label48).set_TabIndex(7);
			((Control)label48).set_Text("Start longitude");
			((Control)textBox35).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GMapName5", true, (DataSourceUpdateMode)1));
			((Control)textBox35).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox35).set_Location(new Point(306, 488));
			((Control)textBox35).set_Name("textBox35");
			((Control)textBox35).set_Size(new Size(103, 20));
			((Control)textBox35).set_TabIndex(52);
			((Control)textBox35).set_Text(Settings.Default.GMapName5);
			((Control)textBox35).add_Leave((EventHandler)textBox35_Leave);
			((Control)updnEndLong).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Graze_DefaultEndLongitude", true, (DataSourceUpdateMode)1));
			((Control)updnEndLong).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnEndLong).set_Location(new Point(228, 209));
			updnEndLong.set_Maximum(new decimal(new int[4] { 200, 0, 0, 0 }));
			updnEndLong.set_Minimum(new decimal(new int[4] { 200, 0, 0, -2147483648 }));
			((Control)updnEndLong).set_Name("updnEndLong");
			((Control)updnEndLong).set_Size(new Size(60, 20));
			((Control)updnEndLong).set_TabIndex(10);
			updnEndLong.set_Value(Settings.Default.Graze_DefaultEndLongitude);
			((Control)numericUpDown29).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapS4", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown29).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown29).set_Location(new Point(640, 468));
			numericUpDown29.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown29.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown29).set_Name("numericUpDown29");
			((Control)numericUpDown29).set_Size(new Size(49, 20));
			((Control)numericUpDown29).set_TabIndex(51);
			((UpDownBase)numericUpDown29).set_TextAlign((HorizontalAlignment)1);
			numericUpDown29.set_Value(Settings.Default.GMapS4);
			((Control)updnStartLong).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Graze_DefaultStartLongitude", true, (DataSourceUpdateMode)1));
			((Control)updnStartLong).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)updnStartLong).set_Location(new Point(228, 186));
			updnStartLong.set_Maximum(new decimal(new int[4] { 200, 0, 0, 0 }));
			updnStartLong.set_Minimum(new decimal(new int[4] { 200, 0, 0, -2147483648 }));
			((Control)updnStartLong).set_Name("updnStartLong");
			((Control)updnStartLong).set_Size(new Size(60, 20));
			((Control)updnStartLong).set_TabIndex(8);
			updnStartLong.set_Value(Settings.Default.Graze_DefaultStartLongitude);
			((Control)numericUpDown30).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapN4", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown30).set_Location(new Point(578, 468));
			numericUpDown30.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown30.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown30).set_Name("numericUpDown30");
			((Control)numericUpDown30).set_Size(new Size(49, 20));
			((Control)numericUpDown30).set_TabIndex(50);
			((UpDownBase)numericUpDown30).set_TextAlign((HorizontalAlignment)1);
			numericUpDown30.set_Value(Settings.Default.GMapN4);
			((Control)panel3).get_Controls().Add((Control)(object)label107);
			((Control)panel3).get_Controls().Add((Control)(object)optGrazeFormatDDD);
			((Control)panel3).get_Controls().Add((Control)(object)label44);
			((Control)panel3).get_Controls().Add((Control)(object)optGrazeFormatDMS);
			((Control)panel3).get_Controls().Add((Control)(object)optGrazeFormatDMM);
			((Control)panel3).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)panel3).set_Location(new Point(9, 244));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(532, 21));
			((Control)panel3).set_TabIndex(15);
			((Control)label107).set_AutoSize(true);
			((Control)label107).set_Location(new Point(395, 4));
			((Control)label107).set_Name("label107");
			((Control)label107).set_Size(new Size(121, 13));
			((Control)label107).set_TabIndex(4);
			((Control)label107).set_Text("(double-click to change)");
			((Control)optGrazeFormatDDD).set_AutoSize(true);
			optGrazeFormatDDD.set_Checked(Settings.Default.Grazes_DDDcoords);
			((Control)optGrazeFormatDDD).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Grazes_DDDcoords", true, (DataSourceUpdateMode)1));
			((Control)optGrazeFormatDDD).set_Location(new Point(317, 1));
			((Control)optGrazeFormatDDD).set_Name("optGrazeFormatDDD");
			((Control)optGrazeFormatDDD).set_Size(new Size(52, 17));
			((Control)optGrazeFormatDDD).set_TabIndex(3);
			((Control)optGrazeFormatDDD).set_Text("d.ddd");
			((ButtonBase)optGrazeFormatDDD).set_UseVisualStyleBackColor(true);
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Location(new Point(23, 3));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(165, 13));
			((Control)label44).set_TabIndex(0);
			((Control)label44).set_Text("Format for graze path coordinates");
			((Control)optGrazeFormatDMS).set_AutoSize(true);
			optGrazeFormatDMS.set_Checked(Settings.Default.Grazes_DMScoords);
			((Control)optGrazeFormatDMS).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Grazes_DMScoords", true, (DataSourceUpdateMode)1));
			((Control)optGrazeFormatDMS).set_Location(new Point(202, 1));
			((Control)optGrazeFormatDMS).set_Name("optGrazeFormatDMS");
			((Control)optGrazeFormatDMS).set_Size(new Size(50, 17));
			((Control)optGrazeFormatDMS).set_TabIndex(1);
			optGrazeFormatDMS.set_TabStop(true);
			((Control)optGrazeFormatDMS).set_Text("d m s");
			((ButtonBase)optGrazeFormatDMS).set_UseVisualStyleBackColor(true);
			((Control)optGrazeFormatDMM).set_AutoSize(true);
			optGrazeFormatDMM.set_Checked(Settings.Default.Grazes_DMMcoords);
			((Control)optGrazeFormatDMM).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "Grazes_DMMcoords", true, (DataSourceUpdateMode)1));
			((Control)optGrazeFormatDMM).set_Location(new Point(258, 1));
			((Control)optGrazeFormatDMM).set_Name("optGrazeFormatDMM");
			((Control)optGrazeFormatDMM).set_Size(new Size(53, 17));
			((Control)optGrazeFormatDMM).set_TabIndex(2);
			((Control)optGrazeFormatDMM).set_Text("d m.m");
			((ButtonBase)optGrazeFormatDMM).set_UseVisualStyleBackColor(true);
			((Control)numericUpDown31).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapE4", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown31).set_Location(new Point(503, 468));
			numericUpDown31.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown31.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown31).set_Name("numericUpDown31");
			((Control)numericUpDown31).set_Size(new Size(56, 20));
			((Control)numericUpDown31).set_TabIndex(49);
			((UpDownBase)numericUpDown31).set_TextAlign((HorizontalAlignment)1);
			numericUpDown31.set_Value(Settings.Default.GMapE4);
			((Control)chkGrazeProfileKM_Mi).set_AutoSize(true);
			chkGrazeProfileKM_Mi.set_CheckAlign(ContentAlignment.MiddleRight);
			chkGrazeProfileKM_Mi.set_Checked(Settings.Default.GrazeProfile_in_km);
			chkGrazeProfileKM_Mi.set_CheckState((CheckState)1);
			((Control)chkGrazeProfileKM_Mi).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "GrazeProfile_in_km", true, (DataSourceUpdateMode)1));
			((Control)chkGrazeProfileKM_Mi).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)chkGrazeProfileKM_Mi).set_Location(new Point(32, 271));
			((Control)chkGrazeProfileKM_Mi).set_Name("chkGrazeProfileKM_Mi");
			((Control)chkGrazeProfileKM_Mi).set_Size(new Size(228, 17));
			((Control)chkGrazeProfileKM_Mi).set_TabIndex(16);
			((Control)chkGrazeProfileKM_Mi).set_Text("Graze profile vertical scale in km (not miles)");
			((ButtonBase)chkGrazeProfileKM_Mi).set_UseVisualStyleBackColor(true);
			((Control)numericUpDown32).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapW4", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown32).set_Location(new Point(436, 468));
			numericUpDown32.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown32.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown32).set_Name("numericUpDown32");
			((Control)numericUpDown32).set_Size(new Size(56, 20));
			((Control)numericUpDown32).set_TabIndex(48);
			((UpDownBase)numericUpDown32).set_TextAlign((HorizontalAlignment)1);
			numericUpDown32.set_Value(Settings.Default.GMapW4);
			((Control)label43).set_AutoSize(true);
			((Control)label43).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label43).set_Location(new Point(150, 150));
			((Control)label43).set_Name("label43");
			((Control)label43).set_Size(new Size(76, 13));
			((Control)label43).set_TabIndex(4);
			((Control)label43).set_Text("stars <4.5 (km)");
			((Control)textBox34).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GMapName4", true, (DataSourceUpdateMode)1));
			((Control)textBox34).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox34).set_Location(new Point(306, 468));
			((Control)textBox34).set_Name("textBox34");
			((Control)textBox34).set_Size(new Size(103, 20));
			((Control)textBox34).set_TabIndex(47);
			((Control)textBox34).set_Text(Settings.Default.GMapName4);
			((Control)textBox34).add_Leave((EventHandler)textBox34_Leave);
			((Control)updnGraze45).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Graze_TravelDistance_45", true, (DataSourceUpdateMode)1));
			((Control)updnGraze45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnGraze45.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGraze45).set_Location(new Point(228, 146));
			updnGraze45.set_Maximum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnGraze45.set_Minimum(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGraze45).set_Name("updnGraze45");
			((Control)updnGraze45).set_Size(new Size(52, 20));
			((Control)updnGraze45).set_TabIndex(5);
			((UpDownBase)updnGraze45).set_TextAlign((HorizontalAlignment)2);
			updnGraze45.set_Value(Settings.Default.Graze_TravelDistance_45);
			((Control)numericUpDown25).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapS3", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown25).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown25).set_Location(new Point(640, 448));
			numericUpDown25.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown25.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown25).set_Name("numericUpDown25");
			((Control)numericUpDown25).set_Size(new Size(49, 20));
			((Control)numericUpDown25).set_TabIndex(46);
			((UpDownBase)numericUpDown25).set_TextAlign((HorizontalAlignment)1);
			numericUpDown25.set_Value(Settings.Default.GMapS3);
			((Control)label42).set_AutoSize(true);
			((Control)label42).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label42).set_Location(new Point(33, 108));
			((Control)label42).set_Name("label42");
			((Control)label42).set_Size(new Size(110, 13));
			((Control)label42).set_TabIndex(1);
			((Control)label42).set_Text("Graze travel distance:");
			((Control)numericUpDown26).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapN3", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown26).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown26).set_Location(new Point(578, 448));
			numericUpDown26.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown26.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown26).set_Name("numericUpDown26");
			((Control)numericUpDown26).set_Size(new Size(49, 20));
			((Control)numericUpDown26).set_TabIndex(45);
			((UpDownBase)numericUpDown26).set_TextAlign((HorizontalAlignment)1);
			((Control)updnGraze65).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "Graze_TravelDistance_65", true, (DataSourceUpdateMode)1));
			((Control)updnGraze65).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			updnGraze65.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGraze65).set_Location(new Point(228, 120));
			updnGraze65.set_Maximum(new decimal(new int[4] { 2000, 0, 0, 0 }));
			updnGraze65.set_Minimum(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGraze65).set_Name("updnGraze65");
			((Control)updnGraze65).set_Size(new Size(52, 20));
			((Control)updnGraze65).set_TabIndex(3);
			((UpDownBase)updnGraze65).set_TextAlign((HorizontalAlignment)2);
			updnGraze65.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)numericUpDown27).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapE3", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown27).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown27).set_Location(new Point(503, 448));
			numericUpDown27.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown27.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown27).set_Name("numericUpDown27");
			((Control)numericUpDown27).set_Size(new Size(56, 20));
			((Control)numericUpDown27).set_TabIndex(44);
			((UpDownBase)numericUpDown27).set_TextAlign((HorizontalAlignment)1);
			((Control)textBox32).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GMapName1", true, (DataSourceUpdateMode)1));
			((Control)textBox32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox32).set_Location(new Point(306, 408));
			((Control)textBox32).set_Name("textBox32");
			((Control)textBox32).set_Size(new Size(103, 20));
			((Control)textBox32).set_TabIndex(32);
			((Control)textBox32).add_Leave((EventHandler)textBox32_Leave);
			((Control)numericUpDown28).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapW3", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown28).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown28).set_Location(new Point(436, 448));
			numericUpDown28.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown28.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown28).set_Name("numericUpDown28");
			((Control)numericUpDown28).set_Size(new Size(56, 20));
			((Control)numericUpDown28).set_TabIndex(43);
			((UpDownBase)numericUpDown28).set_TextAlign((HorizontalAlignment)1);
			((Control)textBox30).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GMapName0", true, (DataSourceUpdateMode)1));
			((Control)textBox30).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox30).set_Location(new Point(306, 388));
			((Control)textBox30).set_Name("textBox30");
			((Control)textBox30).set_Size(new Size(103, 20));
			((Control)textBox30).set_TabIndex(27);
			((Control)textBox30).set_Text(Settings.Default.GMapName0);
			((Control)textBox30).add_Leave((EventHandler)textBox30_Leave);
			((Control)textBox33).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GMapName3", true, (DataSourceUpdateMode)1));
			((Control)textBox33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox33).set_Location(new Point(306, 448));
			((Control)textBox33).set_Name("textBox33");
			((Control)textBox33).set_Size(new Size(103, 20));
			((Control)textBox33).set_TabIndex(42);
			((Control)textBox33).add_Leave((EventHandler)textBox33_Leave);
			((Control)numericUpDown13).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapW0", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown13).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown13).set_Location(new Point(436, 388));
			numericUpDown13.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown13.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown13).set_Name("numericUpDown13");
			((Control)numericUpDown13).set_Size(new Size(56, 20));
			((Control)numericUpDown13).set_TabIndex(28);
			((UpDownBase)numericUpDown13).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown21).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapS1", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown21).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown21).set_Location(new Point(640, 408));
			numericUpDown21.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown21.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown21).set_Name("numericUpDown21");
			((Control)numericUpDown21).set_Size(new Size(49, 20));
			((Control)numericUpDown21).set_TabIndex(36);
			((UpDownBase)numericUpDown21).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown14).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapE0", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown14).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown14).set_Location(new Point(503, 388));
			numericUpDown14.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown14.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown14).set_Name("numericUpDown14");
			((Control)numericUpDown14).set_Size(new Size(56, 20));
			((Control)numericUpDown14).set_TabIndex(29);
			((UpDownBase)numericUpDown14).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown22).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapN1", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown22).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown22).set_Location(new Point(578, 408));
			numericUpDown22.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown22.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown22).set_Name("numericUpDown22");
			((Control)numericUpDown22).set_Size(new Size(49, 20));
			((Control)numericUpDown22).set_TabIndex(35);
			((UpDownBase)numericUpDown22).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown15).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapN0", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown15).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown15).set_Location(new Point(578, 388));
			numericUpDown15.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown15.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown15).set_Name("numericUpDown15");
			((Control)numericUpDown15).set_Size(new Size(49, 20));
			((Control)numericUpDown15).set_TabIndex(30);
			((UpDownBase)numericUpDown15).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown23).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapE1", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown23).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown23).set_Location(new Point(503, 408));
			numericUpDown23.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown23.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown23).set_Name("numericUpDown23");
			((Control)numericUpDown23).set_Size(new Size(56, 20));
			((Control)numericUpDown23).set_TabIndex(34);
			((UpDownBase)numericUpDown23).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown16).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapS0", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown16).set_Location(new Point(640, 388));
			numericUpDown16.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown16.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown16).set_Name("numericUpDown16");
			((Control)numericUpDown16).set_Size(new Size(49, 20));
			((Control)numericUpDown16).set_TabIndex(31);
			((UpDownBase)numericUpDown16).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown24).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapW1", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown24).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown24).set_Location(new Point(436, 408));
			numericUpDown24.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown24.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown24).set_Name("numericUpDown24");
			((Control)numericUpDown24).set_Size(new Size(56, 20));
			((Control)numericUpDown24).set_TabIndex(33);
			((UpDownBase)numericUpDown24).set_TextAlign((HorizontalAlignment)1);
			((Control)textBox31).get_DataBindings().Add(new Binding("Text", (object)Settings.Default, "GMapName2", true, (DataSourceUpdateMode)1));
			((Control)textBox31).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)textBox31).set_Location(new Point(306, 428));
			((Control)textBox31).set_Name("textBox31");
			((Control)textBox31).set_Size(new Size(103, 20));
			((Control)textBox31).set_TabIndex(37);
			((Control)textBox31).add_Leave((EventHandler)textBox31_Leave);
			((Control)numericUpDown20).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapW2", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown20).set_Location(new Point(436, 428));
			numericUpDown20.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown20.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown20).set_Name("numericUpDown20");
			((Control)numericUpDown20).set_Size(new Size(56, 20));
			((Control)numericUpDown20).set_TabIndex(38);
			((UpDownBase)numericUpDown20).set_TextAlign((HorizontalAlignment)1);
			numericUpDown20.set_Value(Settings.Default.GMapW2);
			((Control)numericUpDown17).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapS2", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown17).set_Location(new Point(640, 428));
			numericUpDown17.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown17.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown17).set_Name("numericUpDown17");
			((Control)numericUpDown17).set_Size(new Size(49, 20));
			((Control)numericUpDown17).set_TabIndex(41);
			((UpDownBase)numericUpDown17).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown19).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapE2", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown19).set_Location(new Point(503, 428));
			numericUpDown19.set_Maximum(new decimal(new int[4] { 180, 0, 0, 0 }));
			numericUpDown19.set_Minimum(new decimal(new int[4] { 180, 0, 0, -2147483648 }));
			((Control)numericUpDown19).set_Name("numericUpDown19");
			((Control)numericUpDown19).set_Size(new Size(56, 20));
			((Control)numericUpDown19).set_TabIndex(39);
			((UpDownBase)numericUpDown19).set_TextAlign((HorizontalAlignment)1);
			((Control)numericUpDown18).get_DataBindings().Add(new Binding("Value", (object)Settings.Default, "GMapN2", true, (DataSourceUpdateMode)1));
			((Control)numericUpDown18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)numericUpDown18).set_Location(new Point(578, 428));
			numericUpDown18.set_Maximum(new decimal(new int[4] { 90, 0, 0, 0 }));
			numericUpDown18.set_Minimum(new decimal(new int[4] { 90, 0, 0, -2147483648 }));
			((Control)numericUpDown18).set_Name("numericUpDown18");
			((Control)numericUpDown18).set_Size(new Size(49, 20));
			((Control)numericUpDown18).set_TabIndex(40);
			((UpDownBase)numericUpDown18).set_TextAlign((HorizontalAlignment)1);
			((FileDialog)openFileDialog1).set_FileName("openFileDialog1");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[4]
			{
				(ToolStripItem)undoSessionChangesToolStripMenuItem,
				(ToolStripItem)resstToDefaultsToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)saveAndExitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(804, 24));
			((Control)menuStrip1).set_TabIndex(2);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)undoSessionChangesToolStripMenuItem).set_Image((Image)Resources.KEY05);
			((ToolStripItem)undoSessionChangesToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)undoSessionChangesToolStripMenuItem).set_Name("undoSessionChangesToolStripMenuItem");
			((ToolStripItem)undoSessionChangesToolStripMenuItem).set_Size(new Size(170, 20));
			((ToolStripItem)undoSessionChangesToolStripMenuItem).set_Text("Undo session changes      ");
			((ToolStripItem)undoSessionChangesToolStripMenuItem).add_Click((EventHandler)undoSessionChangesToolStripMenuItem_Click);
			((ToolStripItem)resstToDefaultsToolStripMenuItem).set_Image((Image)Resources.Home);
			((ToolStripItem)resstToDefaultsToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)resstToDefaultsToolStripMenuItem).set_Name("resstToDefaultsToolStripMenuItem");
			((ToolStripItem)resstToDefaultsToolStripMenuItem).set_Size(new Size(113, 20));
			((ToolStripItem)resstToDefaultsToolStripMenuItem).set_Text("Reset ALL         ");
			((ToolStripItem)resstToDefaultsToolStripMenuItem).add_Click((EventHandler)resstToDefaultsToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(81, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help       ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)saveAndExitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)saveAndExitToolStripMenuItem).set_Name("saveAndExitToolStripMenuItem");
			((ToolStripItem)saveAndExitToolStripMenuItem).set_Size(new Size(116, 20));
			((ToolStripItem)saveAndExitToolStripMenuItem).set_Text("Save and Exit    ");
			((ToolStripItem)saveAndExitToolStripMenuItem).add_Click((EventHandler)saveAndExitToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(804, 593));
			((Control)this).get_Controls().Add((Control)(object)panel1);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationDefaultsDefaults", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Icon((Icon)componentResourceManager.GetObject("$this.Icon"));
			((Form)this).set_Location(Settings.Default.LocationDefaultsDefaults);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Form)this).set_MaximizeBox(false);
			((Control)this).set_Name("Defaults");
			((Form)this).set_StartPosition((FormStartPosition)0);
			((Control)this).set_Text("User settings");
			((Form)this).add_FormClosing(new FormClosingEventHandler(Defaults_FormClosing));
			((Form)this).add_Load((EventHandler)Defaults_Load);
			((Control)this).add_Resize((EventHandler)Defaults_Resize);
			((Control)grpHomeLocation).ResumeLayout(false);
			((Control)grpHomeLocation).PerformLayout();
			((Control)grpURLs).ResumeLayout(false);
			((Control)grpURLs).PerformLayout();
			((Control)panel4).ResumeLayout(false);
			((Control)panel4).PerformLayout();
			((ISupportInitialize)numericUpDown57).EndInit();
			((Control)grpEmailAdvanced).ResumeLayout(false);
			((Control)grpEmailAdvanced).PerformLayout();
			((Control)grpStarChartInitialise).ResumeLayout(false);
			((Control)grpStarChartInitialise).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)grpC2A).ResumeLayout(false);
			((Control)grpC2A).PerformLayout();
			((ISupportInitialize)pictureBox3).EndInit();
			((Control)grpEmails).ResumeLayout(false);
			((Control)grpEmails).PerformLayout();
			((Control)grpTest).ResumeLayout(false);
			((Control)grpTest).PerformLayout();
			((Control)grpUpdates).ResumeLayout(false);
			((Control)grpUpdates).PerformLayout();
			((Control)grpAsteroidMultiPathMaps).ResumeLayout(false);
			((Control)grpAsteroidMultiPathMaps).PerformLayout();
			((ISupportInitialize)updnWest).EndInit();
			((ISupportInitialize)updnEast).EndInit();
			((ISupportInitialize)updnNorth).EndInit();
			((ISupportInitialize)updnSouth).EndInit();
			((ISupportInitialize)numericUpDown4).EndInit();
			((ISupportInitialize)numericUpDown3).EndInit();
			((ISupportInitialize)numericUpDown2).EndInit();
			((ISupportInitialize)numericUpDown1).EndInit();
			((ISupportInitialize)numericUpDown8).EndInit();
			((ISupportInitialize)numericUpDown7).EndInit();
			((ISupportInitialize)numericUpDown56).EndInit();
			((ISupportInitialize)numericUpDown6).EndInit();
			((ISupportInitialize)numericUpDown55).EndInit();
			((ISupportInitialize)numericUpDown5).EndInit();
			((ISupportInitialize)numericUpDown54).EndInit();
			((ISupportInitialize)numericUpDown12).EndInit();
			((ISupportInitialize)numericUpDown53).EndInit();
			((ISupportInitialize)numericUpDown11).EndInit();
			((ISupportInitialize)numericUpDown52).EndInit();
			((ISupportInitialize)numericUpDown10).EndInit();
			((ISupportInitialize)numericUpDown51).EndInit();
			((ISupportInitialize)numericUpDown9).EndInit();
			((ISupportInitialize)numericUpDown50).EndInit();
			((ISupportInitialize)numericUpDown49).EndInit();
			((ISupportInitialize)numericUpDown48).EndInit();
			((ISupportInitialize)numericUpDown47).EndInit();
			((ISupportInitialize)numericUpDown46).EndInit();
			((ISupportInitialize)numericUpDown41).EndInit();
			((ISupportInitialize)numericUpDown45).EndInit();
			((ISupportInitialize)numericUpDown42).EndInit();
			((ISupportInitialize)numericUpDown44).EndInit();
			((ISupportInitialize)numericUpDown43).EndInit();
			((Control)grpLunarReports).ResumeLayout(false);
			((Control)grpLunarReports).PerformLayout();
			((ISupportInitialize)updnEOPReminder).EndInit();
			((ISupportInitialize)updnCentury).EndInit();
			((Control)grpAdministrator).ResumeLayout(false);
			((Control)grpAdministrator).PerformLayout();
			((Control)grpSiteFiles).ResumeLayout(false);
			((Control)grpSiteFiles).PerformLayout();
			((ISupportInitialize)updnMagCorrection).EndInit();
			((ISupportInitialize)updnTelescopeAperture).EndInit();
			((ISupportInitialize)updnGrazeTtravelDistance).EndInit();
			((Control)grpFileSaveGraphics).ResumeLayout(false);
			((Control)grpFileSaveGraphics).PerformLayout();
			((ISupportInitialize)updnFontSize).EndInit();
			((ISupportInitialize)updnLineThickness).EndInit();
			((Control)grpAsteroidSearchDisplay).ResumeLayout(false);
			((Control)grpAsteroidSearchDisplay).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((ISupportInitialize)updnPlutoDecCorrn).EndInit();
			((ISupportInitialize)updnPlutoRACorrn).EndInit();
			((ISupportInitialize)updnDAteRangeInAsteroidASelection).EndInit();
			((ISupportInitialize)updnMaximumSearchEvents).EndInit();
			((ISupportInitialize)updnTitanDiameter).EndInit();
			((ISupportInitialize)updnTritonDiameter).EndInit();
			((ISupportInitialize)updnPlutoDiameter).EndInit();
			((ISupportInitialize)updnUranusDiameter).EndInit();
			((ISupportInitialize)updnJupiterDiameter).EndInit();
			((ISupportInitialize)updnSaturnDiameter).EndInit();
			((ISupportInitialize)updnUncertainty).EndInit();
			((ISupportInitialize)updnNeptuneDiameter).EndInit();
			((ISupportInitialize)updnUranus).EndInit();
			((ISupportInitialize)updnMars).EndInit();
			((ISupportInitialize)updnJupiter).EndInit();
			((ISupportInitialize)updnNeptune).EndInit();
			((ISupportInitialize)updnPluto).EndInit();
			((ISupportInitialize)updnSaturn).EndInit();
			((ISupportInitialize)updnVenus).EndInit();
			((ISupportInitialize)updnMercury).EndInit();
			((Control)grpGoogleEarth).ResumeLayout(false);
			((Control)grpGoogleEarth).PerformLayout();
			((ISupportInitialize)numericUpDown59).EndInit();
			((ISupportInitialize)numericUpDown58).EndInit();
			((Control)grpOptionalCatalogues).ResumeLayout(false);
			((Control)grpOptionalCatalogues).PerformLayout();
			((ISupportInitialize)pictureBox2).EndInit();
			((ISupportInitialize)pictureBox1).EndInit();
			((Control)grpLunarOccultations).ResumeLayout(false);
			((Control)grpLunarOccultations).PerformLayout();
			((ISupportInitialize)updnMaxSep).EndInit();
			((ISupportInitialize)updnMinSep).EndInit();
			((ISupportInitialize)numericUpDown37).EndInit();
			((ISupportInitialize)numericUpDown38).EndInit();
			((ISupportInitialize)numericUpDown39).EndInit();
			((ISupportInitialize)numericUpDown40).EndInit();
			((ISupportInitialize)numericUpDown33).EndInit();
			((ISupportInitialize)updnAlt).EndInit();
			((ISupportInitialize)numericUpDown34).EndInit();
			((ISupportInitialize)updnStep).EndInit();
			((ISupportInitialize)numericUpDown35).EndInit();
			((ISupportInitialize)numericUpDown36).EndInit();
			((ISupportInitialize)updnEndLong).EndInit();
			((ISupportInitialize)numericUpDown29).EndInit();
			((ISupportInitialize)updnStartLong).EndInit();
			((ISupportInitialize)numericUpDown30).EndInit();
			((Control)panel3).ResumeLayout(false);
			((Control)panel3).PerformLayout();
			((ISupportInitialize)numericUpDown31).EndInit();
			((ISupportInitialize)numericUpDown32).EndInit();
			((ISupportInitialize)updnGraze45).EndInit();
			((ISupportInitialize)numericUpDown25).EndInit();
			((ISupportInitialize)numericUpDown26).EndInit();
			((ISupportInitialize)updnGraze65).EndInit();
			((ISupportInitialize)numericUpDown27).EndInit();
			((ISupportInitialize)numericUpDown28).EndInit();
			((ISupportInitialize)numericUpDown13).EndInit();
			((ISupportInitialize)numericUpDown21).EndInit();
			((ISupportInitialize)numericUpDown14).EndInit();
			((ISupportInitialize)numericUpDown22).EndInit();
			((ISupportInitialize)numericUpDown15).EndInit();
			((ISupportInitialize)numericUpDown23).EndInit();
			((ISupportInitialize)numericUpDown16).EndInit();
			((ISupportInitialize)numericUpDown24).EndInit();
			((ISupportInitialize)numericUpDown20).EndInit();
			((ISupportInitialize)numericUpDown17).EndInit();
			((ISupportInitialize)numericUpDown19).EndInit();
			((ISupportInitialize)numericUpDown18).EndInit();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
