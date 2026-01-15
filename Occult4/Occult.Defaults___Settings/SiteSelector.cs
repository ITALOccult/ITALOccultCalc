using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Defaults___Settings
{
	public class SiteSelector : Form
	{
		private Sites S;

		private Sites CurrentSite = new Sites();

		private List<Sites> AllSites = new List<Sites>();

		private string SiteFileName;

		private bool FormCreated;

		private IContainer components;

		private ComboBox cmbSiteFiles;

		private ListBox lstSites;

		private Label label1;

		private Label label2;

		private Label label3;

		private Label label4;

		public TextBox txtLongitude;

		public TextBox txtLatitude;

		private Button cmdOK;

		private Button cmdCancel;

		private Button cmdHome;

		private Label label5;

		private Label label6;

		private Label label7;

		private Label lblHomeLongitude;

		private Label lblHomeLatitude;

		public TextBox txtAperture;

		private Label Label18;

		private Label lblAperture;

		private Label label9;

		public SiteSelector()
		{
			InitializeComponent();
		}

		private void SiteSelector_Load(object sender, EventArgs e)
		{
			//IL_0063: Unknown result type (might be due to invalid IL or missing references)
			string[] files = Directory.GetFiles(Utilities.AppPath + "\\Sites", "*.site");
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
			string siteFileAsteroidSelect = Settings.Default.SiteFileAsteroidSelect;
			for (int j = 0; j < cmbSiteFiles.get_Items().get_Count(); j++)
			{
				if (siteFileAsteroidSelect == cmbSiteFiles.get_Items().get_Item(j).ToString())
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(j);
					break;
				}
			}
			((Control)lblHomeLongitude).set_Text(Utilities.GetSiteLongitude().ToString() + "°");
			((Control)lblHomeLatitude).set_Text(Utilities.GetSiteLatitude().ToString() + "°");
			((Control)lblAperture).set_Text(Utilities.GetSiteAperture().ToString() + " cm");
			FormCreated = true;
		}

		private void cmbSiteFiles_SelectedIndexChanged(object sender, EventArgs e)
		{
			AllSites.Clear();
			SiteFileName = Path.GetFileName(cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()).ToString());
			StreamReader streamReader = new StreamReader(Utilities.AppPath + "\\sites\\" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()));
			do
			{
				S = new Sites();
				S.Read_SiteFile(streamReader.ReadLine());
				AllSites.Add(S);
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			Sites.SortField = 0;
			AllSites.Sort();
			lstSites.get_Items().Clear();
			for (int i = 0; i < AllSites.Count; i++)
			{
				lstSites.get_Items().Add((object)AllSites[i].Name);
			}
			if (AllSites.Count > 0)
			{
				((ListControl)lstSites).set_SelectedIndex(0);
			}
			if (FormCreated)
			{
				Settings.Default.SiteFileAsteroidSelect = SiteFileName;
			}
		}

		private void cmdHome_Click(object sender, EventArgs e)
		{
			((Control)txtLongitude).set_Text(Utilities.GetSiteLongitude());
			((Control)txtLatitude).set_Text(Utilities.GetSiteLatitude());
			((Control)txtAperture).set_Text(Utilities.GetSiteAperture());
			((Form)this).set_DialogResult((DialogResult)1);
		}

		private void lstSites_SelectedIndexChanged(object sender, EventArgs e)
		{
			((Control)txtLongitude).set_Text(string.Format("{0,4:f2}", AllSites[((ListControl)lstSites).get_SelectedIndex()].Longitude));
			((Control)txtLatitude).set_Text(string.Format("{0,4:f2}", AllSites[((ListControl)lstSites).get_SelectedIndex()].Latitude));
			((Control)txtAperture).set_Text(string.Format("{0,3:f0}", AllSites[((ListControl)lstSites).get_SelectedIndex()].ApertureCM));
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)1);
		}

		private void lstSites_DoubleClick(object sender, EventArgs e)
		{
			((Control)txtLongitude).set_Text(string.Format("{0,4:f2}", AllSites[((ListControl)lstSites).get_SelectedIndex()].Longitude));
			((Control)txtLatitude).set_Text(string.Format("{0,4:f2}", AllSites[((ListControl)lstSites).get_SelectedIndex()].Latitude));
			((Form)this).set_DialogResult((DialogResult)1);
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			((Form)this).set_DialogResult((DialogResult)2);
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
			cmbSiteFiles = new ComboBox();
			lstSites = new ListBox();
			label1 = new Label();
			label2 = new Label();
			txtLongitude = new TextBox();
			txtLatitude = new TextBox();
			label3 = new Label();
			label4 = new Label();
			cmdOK = new Button();
			cmdCancel = new Button();
			cmdHome = new Button();
			label5 = new Label();
			label6 = new Label();
			label7 = new Label();
			lblHomeLongitude = new Label();
			lblHomeLatitude = new Label();
			txtAperture = new TextBox();
			Label18 = new Label();
			lblAperture = new Label();
			label9 = new Label();
			((Control)this).SuspendLayout();
			((Control)cmbSiteFiles).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(17, 32));
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(132, 21));
			cmbSiteFiles.set_Sorted(true);
			((Control)cmbSiteFiles).set_TabIndex(90);
			cmbSiteFiles.add_SelectedIndexChanged((EventHandler)cmbSiteFiles_SelectedIndexChanged);
			((ListControl)lstSites).set_FormattingEnabled(true);
			((Control)lstSites).set_Location(new Point(167, 32));
			((Control)lstSites).set_Name("lstSites");
			((Control)lstSites).set_Size(new Size(134, 316));
			((Control)lstSites).set_TabIndex(91);
			lstSites.add_SelectedIndexChanged((EventHandler)lstSites_SelectedIndexChanged);
			((Control)lstSites).add_DoubleClick((EventHandler)lstSites_DoubleClick);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(20, 16));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(41, 13));
			((Control)label1).set_TabIndex(92);
			((Control)label1).set_Text("Site file");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(172, 16));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(30, 13));
			((Control)label2).set_TabIndex(93);
			((Control)label2).set_Text("Sites");
			((Control)txtLongitude).set_Location(new Point(167, 375));
			((Control)txtLongitude).set_Name("txtLongitude");
			((Control)txtLongitude).set_Size(new Size(51, 20));
			((Control)txtLongitude).set_TabIndex(94);
			((Control)txtLatitude).set_Location(new Point(236, 375));
			((Control)txtLatitude).set_Name("txtLatitude");
			((Control)txtLatitude).set_Size(new Size(51, 20));
			((Control)txtLatitude).set_TabIndex(95);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(236, 359));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(45, 13));
			((Control)label3).set_TabIndex(96);
			((Control)label3).set_Text("Latitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(167, 359));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(54, 13));
			((Control)label4).set_TabIndex(97);
			((Control)label4).set_Text("Longitude");
			((Control)cmdOK).set_Location(new Point(81, 425));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(53, 23));
			((Control)cmdOK).set_TabIndex(98);
			((Control)cmdOK).set_Text("OK");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cmdCancel).set_Location(new Point(172, 425));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(53, 23));
			((Control)cmdCancel).set_TabIndex(99);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdHome).set_Location(new Point(17, 274));
			((Control)cmdHome).set_Name("cmdHome");
			((Control)cmdHome).set_Size(new Size(114, 29));
			((Control)cmdHome).set_TabIndex(100);
			((Control)cmdHome).set_Text("Use 'Home' site");
			((ButtonBase)cmdHome).set_UseVisualStyleBackColor(true);
			((Control)cmdHome).add_Click((EventHandler)cmdHome_Click);
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(14, 118));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(82, 104));
			((Control)label5).set_TabIndex(101);
			((Control)label5).set_Text("Select Site file\r\nand a site from\r\nthat file\r\n\r\n- OR -\r\n\r\nclick the 'Home'\r\nsite below.");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(30, 305));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(54, 13));
			((Control)label6).set_TabIndex(102);
			((Control)label6).set_Text("Longitude");
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Location(new Point(30, 318));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(45, 13));
			((Control)label7).set_TabIndex(103);
			((Control)label7).set_Text("Latitude");
			((Control)lblHomeLongitude).set_AutoSize(true);
			((Control)lblHomeLongitude).set_Location(new Point(87, 305));
			((Control)lblHomeLongitude).set_Name("lblHomeLongitude");
			((Control)lblHomeLongitude).set_Size(new Size(38, 13));
			((Control)lblHomeLongitude).set_TabIndex(104);
			((Control)lblHomeLongitude).set_Text("0.000°");
			((Control)lblHomeLatitude).set_AutoSize(true);
			((Control)lblHomeLatitude).set_Location(new Point(87, 318));
			((Control)lblHomeLatitude).set_Name("lblHomeLatitude");
			((Control)lblHomeLatitude).set_Size(new Size(38, 13));
			((Control)lblHomeLatitude).set_TabIndex(105);
			((Control)lblHomeLatitude).set_Text("0.000°");
			((Control)txtAperture).set_Location(new Point(94, 375));
			((Control)txtAperture).set_Name("txtAperture");
			((Control)txtAperture).set_Size(new Size(36, 20));
			((Control)txtAperture).set_TabIndex(106);
			((Control)Label18).set_AutoSize(true);
			((Control)Label18).set_Location(new Point(80, 359));
			((Control)Label18).set_Name("Label18");
			((Control)Label18).set_Size(new Size(70, 13));
			((Control)Label18).set_TabIndex(107);
			((Control)Label18).set_Text("Aperture (cm)");
			((Control)lblAperture).set_AutoSize(true);
			((Control)lblAperture).set_Location(new Point(88, 331));
			((Control)lblAperture).set_Name("lblAperture");
			((Control)lblAperture).set_Size(new Size(36, 13));
			((Control)lblAperture).set_TabIndex(108);
			((Control)lblAperture).set_Text("20 cm");
			((Control)label9).set_AutoSize(true);
			((Control)label9).set_Location(new Point(30, 331));
			((Control)label9).set_Name("label9");
			((Control)label9).set_Size(new Size(50, 13));
			((Control)label9).set_TabIndex(109);
			((Control)label9).set_Text("Aperture ");
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(307, 456));
			((Control)this).get_Controls().Add((Control)(object)label9);
			((Control)this).get_Controls().Add((Control)(object)lblAperture);
			((Control)this).get_Controls().Add((Control)(object)Label18);
			((Control)this).get_Controls().Add((Control)(object)txtAperture);
			((Control)this).get_Controls().Add((Control)(object)lblHomeLatitude);
			((Control)this).get_Controls().Add((Control)(object)lblHomeLongitude);
			((Control)this).get_Controls().Add((Control)(object)label7);
			((Control)this).get_Controls().Add((Control)(object)label6);
			((Control)this).get_Controls().Add((Control)(object)label5);
			((Control)this).get_Controls().Add((Control)(object)cmdHome);
			((Control)this).get_Controls().Add((Control)(object)cmdCancel);
			((Control)this).get_Controls().Add((Control)(object)cmdOK);
			((Control)this).get_Controls().Add((Control)(object)label4);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)txtLatitude);
			((Control)this).get_Controls().Add((Control)(object)txtLongitude);
			((Control)this).get_Controls().Add((Control)(object)label2);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).get_Controls().Add((Control)(object)lstSites);
			((Control)this).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)this).set_Name("SiteSelector");
			((Control)this).set_Text("Select a site for predictions");
			((Form)this).add_Load((EventHandler)SiteSelector_Load);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
