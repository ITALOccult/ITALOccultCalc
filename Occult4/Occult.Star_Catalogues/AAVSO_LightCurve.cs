using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Star_Catalogues
{
	public class AAVSO_LightCurve : Form
	{
		private string VarStar = "";

		private IContainer components;

		private WebBrowser webBrowserAAVSO;

		internal TextBox txtVar;

		private NumericUpDown updnYear;

		private ComboBox cmbMonth;

		private Label label1;

		private Label label2;

		private GroupBox groupBox1;

		private CheckBox chkVisual;

		private CheckBox chkR;

		private CheckBox chkV;

		private CheckBox chkB;

		private CheckBox chkU;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem withLightCurveToolStripMenuItem;

		private ToolStripMenuItem copyToolStripMenuItem;

		private ToolStripMenuItem printToolStripMenuItem;

		private ToolStripMenuItem saveToolStripMenuItem;

		private PictureBox picAAVSO;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		internal Button cmdGetCurve;

		private Label lblGettingData;

		private Button cmdBack;

		private Button cmdForward;

		public AAVSO_LightCurve()
		{
			InitializeComponent();
			webBrowserAAVSO.add_CanGoBackChanged((EventHandler)webBrowserAAVSO_CanGoBackChanged);
			webBrowserAAVSO.add_CanGoForwardChanged((EventHandler)webBrowserAAVSO_CanGoForwardChanged);
		}

		private void AAVSO_LightCurve_Load(object sender, EventArgs e)
		{
			((ListControl)cmbMonth).set_SelectedIndex(0);
			updnYear.set_Value((decimal)(DateTime.Now.Year - 5));
			((Control)picAAVSO).set_Top(((Control)webBrowserAAVSO).get_Top());
			((Control)picAAVSO).set_Left(((Control)webBrowserAAVSO).get_Left());
			((Control)this).set_Text(((Control)this).get_Text() + " : Occult v." + Utilities.OccultVersion_Short);
		}

		private void cmdGetCurve_Click(object sender, EventArgs e)
		{
			GetLightCurve();
		}

		internal void GetLightCurve()
		{
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			double num = Utilities.JD_from_Date(DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day);
			double num2 = Utilities.JD_from_Date((int)updnYear.get_Value(), ((ListControl)cmbMonth).get_SelectedIndex() + 1, 1.0);
			string text = Math.Floor(num - num2).ToString();
			string text2 = Math.Floor(num).ToString();
			VarStar = ((Control)txtVar).get_Text().Trim().Replace(" ", "+")
				.Replace("++", "+")
				.ToUpper();
			string text3 = "off";
			string text4 = "off";
			string text5 = "off";
			string text6 = "off";
			string text7 = "off";
			string text8 = ((int)((double)((Control)picAAVSO).get_Width() * 0.9) - 50).ToString();
			string text9 = (((Control)picAAVSO).get_Height() - 50).ToString();
			if (chkVisual.get_Checked())
			{
				text3 = "on";
			}
			if (chkU.get_Checked())
			{
				text4 = "on";
			}
			if (chkB.get_Checked())
			{
				text5 = "on";
			}
			if (chkV.get_Checked())
			{
				text6 = "on";
			}
			if (chkR.get_Checked())
			{
				text7 = "on";
			}
			((Control)webBrowserAAVSO).set_Visible(false);
			((Control)picAAVSO).set_Visible(false);
			((Control)lblGettingData).set_Visible(true);
			string text10 = "http://www.aavso.org/cgi-bin/lcg.pl?auid=" + VarStar + "&lastdays=" + text + "&start=&stop=" + text2 + "&obscode=&obscode_symbol=2&visual=" + text3 + "&uband=" + text4 + "&bband=" + text5 + "&v=" + text6 + "&r=" + text7 + "&grid=on&pointsize=1&width=" + text8 + "&height=" + text9 + "&mag1=&mag2=&mean=30&vmean=&button_name=Please+Wait...";
			Cursor.set_Current(Cursors.get_WaitCursor());
			Application.DoEvents();
			webBrowserAAVSO.Navigate(text10);
			Cursor.set_Current(Cursors.get_Default());
			Application.DoEvents();
		}

		private void AAVSO_LightCurve_Resize(object sender, EventArgs e)
		{
			PictureBox obj = picAAVSO;
			int height;
			((Control)webBrowserAAVSO).set_Height(height = ((Control)this).get_Height() - 145);
			((Control)obj).set_Height(height);
			PictureBox obj2 = picAAVSO;
			((Control)webBrowserAAVSO).set_Width(height = ((Control)this).get_Width() - 40);
			((Control)obj2).set_Width(height);
		}

		private void copyToolStripMenuItem_Click(object sender, EventArgs e)
		{
			if (((Control)webBrowserAAVSO).get_Visible())
			{
				Clipboard.SetText(webBrowserAAVSO.get_DocumentText());
			}
			else if (((Control)picAAVSO).get_Visible())
			{
				Clipboard.SetImage(picAAVSO.get_Image());
			}
		}

		private void printToolStripMenuItem_Click(object sender, EventArgs e)
		{
		}

		private void saveToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Settings.Default.Save_Var_LightCurve = Output.SaveGraphic(picAAVSO.get_Image(), VarStar, Settings.Default.Save_Var_LightCurve);
		}

		private void webBrowserAAVSO_DocumentCompleted(object sender, WebBrowserDocumentCompletedEventArgs e)
		{
			//IL_0091: Unknown result type (might be due to invalid IL or missing references)
			string documentText = webBrowserAAVSO.get_DocumentText();
			int num = documentText.IndexOf("<img src=\"http://www.aavso.org");
			if (num > 0)
			{
				try
				{
					int num2 = documentText.IndexOf("\"", num) + 1;
					int num3 = documentText.IndexOf("\"", num2);
					string text = documentText.Substring(num2, num3 - num2);
					picAAVSO.Load(text);
					((Control)webBrowserAAVSO).set_Visible(false);
					((Control)picAAVSO).set_Visible(true);
				}
				catch
				{
					((Control)webBrowserAAVSO).set_Visible(true);
					((Control)picAAVSO).set_Visible(false);
					MessageBox.Show("Requested Light Curve is not available.", "No Light Curve");
				}
				((Control)lblGettingData).set_Visible(false);
			}
			else
			{
				((Control)webBrowserAAVSO).set_Visible(true);
				((Control)picAAVSO).set_Visible(false);
				((Control)lblGettingData).set_Visible(false);
			}
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"AAVSO Light Curve");
		}

		private void cmdBack_Click(object sender, EventArgs e)
		{
			webBrowserAAVSO.GoBack();
		}

		private void cmdForward_Click(object sender, EventArgs e)
		{
			webBrowserAAVSO.GoForward();
		}

		private void webBrowserAAVSO_CanGoBackChanged(object sender, EventArgs e)
		{
			((Control)cmdBack).set_Enabled(webBrowserAAVSO.get_CanGoBack());
		}

		private void webBrowserAAVSO_CanGoForwardChanged(object sender, EventArgs e)
		{
			((Control)cmdForward).set_Enabled(webBrowserAAVSO.get_CanGoForward());
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
			//IL_0220: Unknown result type (might be due to invalid IL or missing references)
			//IL_022a: Expected O, but got Unknown
			cmdGetCurve = new Button();
			webBrowserAAVSO = new WebBrowser();
			txtVar = new TextBox();
			updnYear = new NumericUpDown();
			cmbMonth = new ComboBox();
			label1 = new Label();
			label2 = new Label();
			groupBox1 = new GroupBox();
			chkVisual = new CheckBox();
			chkR = new CheckBox();
			chkV = new CheckBox();
			chkB = new CheckBox();
			chkU = new CheckBox();
			menuStrip1 = new MenuStrip();
			withLightCurveToolStripMenuItem = new ToolStripMenuItem();
			copyToolStripMenuItem = new ToolStripMenuItem();
			printToolStripMenuItem = new ToolStripMenuItem();
			saveToolStripMenuItem = new ToolStripMenuItem();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			lblGettingData = new Label();
			cmdForward = new Button();
			cmdBack = new Button();
			picAAVSO = new PictureBox();
			((ISupportInitialize)updnYear).BeginInit();
			((Control)groupBox1).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((ISupportInitialize)picAAVSO).BeginInit();
			((Control)this).SuspendLayout();
			((Control)cmdGetCurve).set_Location(new Point(585, 40));
			((Control)cmdGetCurve).set_Name("cmdGetCurve");
			((Control)cmdGetCurve).set_Size(new Size(104, 33));
			((Control)cmdGetCurve).set_TabIndex(6);
			((Control)cmdGetCurve).set_Text("Get light curve");
			((ButtonBase)cmdGetCurve).set_UseVisualStyleBackColor(true);
			((Control)cmdGetCurve).add_Click((EventHandler)cmdGetCurve_Click);
			((Control)webBrowserAAVSO).set_Location(new Point(12, 97));
			((Control)webBrowserAAVSO).set_MinimumSize(new Size(20, 20));
			((Control)webBrowserAAVSO).set_Name("webBrowserAAVSO");
			((Control)webBrowserAAVSO).set_Size(new Size(840, 424));
			((Control)webBrowserAAVSO).set_TabIndex(9);
			webBrowserAAVSO.add_DocumentCompleted(new WebBrowserDocumentCompletedEventHandler(webBrowserAAVSO_DocumentCompleted));
			((Control)txtVar).set_Location(new Point(60, 53));
			((Control)txtVar).set_Name("txtVar");
			((Control)txtVar).set_Size(new Size(92, 20));
			((Control)txtVar).set_TabIndex(1);
			((Control)updnYear).set_Location(new Point(184, 51));
			updnYear.set_Maximum(new decimal(new int[4] { 2020, 0, 0, 0 }));
			updnYear.set_Minimum(new decimal(new int[4] { 1800, 0, 0, 0 }));
			((Control)updnYear).set_Name("updnYear");
			((Control)updnYear).set_Size(new Size(53, 20));
			((Control)updnYear).set_TabIndex(3);
			updnYear.set_Value(new decimal(new int[4] { 2006, 0, 0, 0 }));
			((ListControl)cmbMonth).set_FormattingEnabled(true);
			cmbMonth.get_Items().AddRange(new object[12]
			{
				"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct",
				"Nov", "Dec"
			});
			((Control)cmbMonth).set_Location(new Point(243, 50));
			((Control)cmbMonth).set_Name("cmbMonth");
			((Control)cmbMonth).set_Size(new Size(43, 21));
			((Control)cmbMonth).set_TabIndex(4);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(181, 33));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(116, 13));
			((Control)label1).set_TabIndex(2);
			((Control)label1).set_Text("Plot observations since");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(63, 36));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(87, 13));
			((Control)label2).set_TabIndex(0);
			((Control)label2).set_Text("Variable identifier");
			((Control)groupBox1).get_Controls().Add((Control)(object)chkVisual);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkR);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkV);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkB);
			((Control)groupBox1).get_Controls().Add((Control)(object)chkU);
			((Control)groupBox1).set_Location(new Point(316, 32));
			((Control)groupBox1).set_Name("groupBox1");
			((Control)groupBox1).set_Size(new Size(143, 59));
			((Control)groupBox1).set_TabIndex(5);
			groupBox1.set_TabStop(false);
			((Control)groupBox1).set_Text("Include");
			((Control)chkVisual).set_AutoSize(true);
			chkVisual.set_Checked(true);
			chkVisual.set_CheckState((CheckState)1);
			((Control)chkVisual).set_Location(new Point(87, 24));
			((Control)chkVisual).set_Name("chkVisual");
			((Control)chkVisual).set_Size(new Size(54, 17));
			((Control)chkVisual).set_TabIndex(4);
			((Control)chkVisual).set_Text("Visual");
			((ButtonBase)chkVisual).set_UseVisualStyleBackColor(true);
			((Control)chkR).set_AutoSize(true);
			chkR.set_Checked(true);
			chkR.set_CheckState((CheckState)1);
			((Control)chkR).set_Location(new Point(48, 34));
			((Control)chkR).set_Name("chkR");
			((Control)chkR).set_Size(new Size(34, 17));
			((Control)chkR).set_TabIndex(3);
			((Control)chkR).set_Text("R");
			((ButtonBase)chkR).set_UseVisualStyleBackColor(true);
			((Control)chkV).set_AutoSize(true);
			chkV.set_Checked(true);
			chkV.set_CheckState((CheckState)1);
			((Control)chkV).set_Location(new Point(48, 17));
			((Control)chkV).set_Name("chkV");
			((Control)chkV).set_Size(new Size(33, 17));
			((Control)chkV).set_TabIndex(2);
			((Control)chkV).set_Text("V");
			((ButtonBase)chkV).set_UseVisualStyleBackColor(true);
			((Control)chkB).set_AutoSize(true);
			((Control)chkB).set_Location(new Point(8, 34));
			((Control)chkB).set_Name("chkB");
			((Control)chkB).set_Size(new Size(33, 17));
			((Control)chkB).set_TabIndex(1);
			((Control)chkB).set_Text("B");
			((ButtonBase)chkB).set_UseVisualStyleBackColor(true);
			((Control)chkU).set_AutoSize(true);
			((Control)chkU).set_Location(new Point(8, 17));
			((Control)chkU).set_Name("chkU");
			((Control)chkU).set_Size(new Size(34, 17));
			((Control)chkU).set_TabIndex(0);
			((Control)chkU).set_Text("U");
			((ButtonBase)chkU).set_UseVisualStyleBackColor(true);
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)withLightCurveToolStripMenuItem,
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(864, 24));
			((Control)menuStrip1).set_TabIndex(0);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripDropDownItem)withLightCurveToolStripMenuItem).get_DropDownItems().AddRange((ToolStripItem[])(object)new ToolStripItem[3]
			{
				(ToolStripItem)copyToolStripMenuItem,
				(ToolStripItem)printToolStripMenuItem,
				(ToolStripItem)saveToolStripMenuItem
			});
			((ToolStripItem)withLightCurveToolStripMenuItem).set_Name("withLightCurveToolStripMenuItem");
			((ToolStripItem)withLightCurveToolStripMenuItem).set_Size(new Size(127, 20));
			((ToolStripItem)withLightCurveToolStripMenuItem).set_Text("with Light Curve...    ");
			((ToolStripItem)copyToolStripMenuItem).set_Image((Image)Resources.Copy);
			((ToolStripItem)copyToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)copyToolStripMenuItem).set_Name("copyToolStripMenuItem");
			((ToolStripItem)copyToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)copyToolStripMenuItem).set_Text("Copy");
			((ToolStripItem)copyToolStripMenuItem).add_Click((EventHandler)copyToolStripMenuItem_Click);
			((ToolStripItem)printToolStripMenuItem).set_Enabled(false);
			((ToolStripItem)printToolStripMenuItem).set_Image((Image)Resources.Print);
			((ToolStripItem)printToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)printToolStripMenuItem).set_Name("printToolStripMenuItem");
			((ToolStripItem)printToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)printToolStripMenuItem).set_Text("Print");
			((ToolStripItem)printToolStripMenuItem).add_Click((EventHandler)printToolStripMenuItem_Click);
			((ToolStripItem)saveToolStripMenuItem).set_Image((Image)Resources.Save);
			((ToolStripItem)saveToolStripMenuItem).set_ImageTransparentColor(Color.Fuchsia);
			((ToolStripItem)saveToolStripMenuItem).set_Name("saveToolStripMenuItem");
			((ToolStripItem)saveToolStripMenuItem).set_Size(new Size(102, 22));
			((ToolStripItem)saveToolStripMenuItem).set_Text("Save");
			((ToolStripItem)saveToolStripMenuItem).add_Click((EventHandler)saveToolStripMenuItem_Click);
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(69, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("Help   ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(62, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("Exit   ");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((Control)lblGettingData).set_AutoSize(true);
			((Control)lblGettingData).set_Font(new Font("Microsoft Sans Serif", 20.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)lblGettingData).set_Location(new Point(60, 264));
			((Control)lblGettingData).set_Name("lblGettingData");
			((Control)lblGettingData).set_Size(new Size(730, 31));
			((Control)lblGettingData).set_TabIndex(8);
			((Control)lblGettingData).set_Text("Retrieving data from the AAVSO Light Curve Generator");
			((Control)cmdForward).set_Enabled(false);
			((ButtonBase)cmdForward).set_Image((Image)Resources.GoLtrHS);
			((Control)cmdForward).set_Location(new Point(768, 44));
			((Control)cmdForward).set_Name("cmdForward");
			((Control)cmdForward).set_Size(new Size(20, 24));
			((Control)cmdForward).set_TabIndex(17);
			((ButtonBase)cmdForward).set_UseVisualStyleBackColor(true);
			((Control)cmdForward).add_Click((EventHandler)cmdForward_Click);
			((Control)cmdBack).set_Enabled(false);
			((ButtonBase)cmdBack).set_Image((Image)Resources.GoRtlHS);
			((Control)cmdBack).set_Location(new Point(742, 44));
			((Control)cmdBack).set_Name("cmdBack");
			((Control)cmdBack).set_Size(new Size(20, 24));
			((Control)cmdBack).set_TabIndex(16);
			((ButtonBase)cmdBack).set_UseVisualStyleBackColor(true);
			((Control)cmdBack).add_Click((EventHandler)cmdBack_Click);
			picAAVSO.set_BorderStyle((BorderStyle)1);
			((Control)picAAVSO).set_Location(new Point(24, 89));
			((Control)picAAVSO).set_Name("picAAVSO");
			((Control)picAAVSO).set_Size(new Size(840, 424));
			picAAVSO.set_TabIndex(15);
			picAAVSO.set_TabStop(false);
			((Control)picAAVSO).set_Visible(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(864, 533));
			((Control)this).get_Controls().Add((Control)(object)cmdForward);
			((Control)this).get_Controls().Add((Control)(object)cmdBack);
			((Control)this).get_Controls().Add((Control)(object)picAAVSO);
			((Control)this).get_Controls().Add((Control)(object)groupBox1);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)cmbMonth);
			((Control)this).get_Controls().Add((Control)(object)updnYear);
			((Control)this).get_Controls().Add((Control)(object)txtVar);
			((Control)this).get_Controls().Add((Control)(object)webBrowserAAVSO);
			((Control)this).get_Controls().Add((Control)(object)cmdGetCurve);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_Controls().Add((Control)(object)lblGettingData);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(500, 300));
			((Control)this).set_Name("AAVSO_LightCurve");
			((Control)this).set_Text("AAVSO Light Curve       [from the AAVSO Light Curve Generator at : http://www.aavso.org/data/lcg/ ]");
			((Form)this).add_Load((EventHandler)AAVSO_LightCurve_Load);
			((Control)this).add_Resize((EventHandler)AAVSO_LightCurve_Resize);
			((ISupportInitialize)updnYear).EndInit();
			((Control)groupBox1).ResumeLayout(false);
			((Control)groupBox1).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((ISupportInitialize)picAAVSO).EndInit();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
