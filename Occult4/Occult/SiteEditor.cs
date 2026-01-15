using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using System.Xml.Serialization;
using Occult.Properties;

namespace Occult
{
	public class SiteEditor : Form
	{
		private readonly string AppPath;

		private Sites S;

		private Sites CurrentSite;

		private List<Sites> AllSites;

		private bool Edit;

		private bool UnsavedChanges;

		private bool DontClose;

		private bool UpdatingData;

		private static string FileName;

		private static string XMLFileName;

		private IContainer components;

		private ListBox lstSiteNames;

		private Label label1;

		private TextBox txtLongitude;

		private TextBox txtLatitude;

		private TextBox txtAlt_m;

		private TextBox txtName;

		private NumericUpDown updnAperture;

		private NumericUpDown updnMagCorrn;

		private NumericUpDown updnGrazeDist;

		private TextBox txtShortName;

		private ComboBox cmbSitePlot;

		private Label label39;

		private Label label36;

		private Label label38;

		private Label label37;

		private TextBox txtLongS;

		private TextBox txtLongM;

		private TextBox txtLongD;

		private TextBox txtLatS;

		private TextBox txtLatM;

		private TextBox txtLatD;

		private Label label20;

		private Label label24;

		private Label label25;

		private Label label2;

		private Label label3;

		private Label label4;

		private Label label5;

		private Label label6;

		private Button cmdClear;

		private GroupBox grpEdit;

		private Button cmdCancel;

		private Button cmdOK;

		private Button cmdEdit;

		private Button cmdAdd;

		private Button cmdDelete;

		private Panel pnlEdit;

		private Label lblLongChar;

		private Label lblShortChars;

		private Label label7;

		private Button cmdSortName;

		private Button cmdSortByLongitude;

		private Button cmdSortByLatitude;

		private Label label12;

		private Label label13;

		private Panel panelList;

		private Button cmdImport;

		private Button cmdOpen;

		private Label label14;

		private ComboBox cmbSiteFiles;

		private Button cmdSaveAs;

		private Button cmdSave;

		private GroupBox grpOpen;

		private CheckBox chkUseXML;

		private Label label15;

		private NumericUpDown updnTimeZone;

		private Label label16;

		private Label label17;

		private ComboBox cmbGoogle;

		private Label label18;

		private Label label19;

		private Panel panelDMS;

		private Panel panelDDD;

		private Label label26;

		private Panel panelDMM;

		private TextBox txtLatMdm;

		private TextBox txtLatDdm;

		private TextBox txtLongMdm;

		private TextBox txtLongDdm;

		private Label label21;

		private Label label22;

		private Panel panel1;

		private RadioButton optDMM;

		private RadioButton optDDD;

		private RadioButton optDMS;

		private Panel panel2;

		private RadioButton optFeet;

		private RadioButton optMeters;

		private TextBox txtAlt_ft;

		private MenuStrip menuStrip1;

		private ToolStripMenuItem helpToolStripMenuItem;

		private ToolStripMenuItem exitToolStripMenuItem;

		public SiteEditor()
		{
			InitializeComponent();
			Panel obj = panelDDD;
			int top;
			((Control)panelDMM).set_Top(top = ((Control)panelDMS).get_Top());
			((Control)obj).set_Top(top);
			Panel obj2 = panelDDD;
			((Control)panelDMM).set_Left(top = ((Control)panelDMS).get_Left());
			((Control)obj2).set_Left(top);
			((Control)txtAlt_ft).set_Top(((Control)txtAlt_m).get_Top());
			SetControlVisibility(State: false);
			AppPath = Utilities.AppPath;
		}

		private void SiteEditor_Load(object sender, EventArgs e)
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
			AltitudeVisibility();
			AllSites = new List<Sites>();
			Sites.SortField = Settings.Default.Sites_EditorSortOrder;
			CurrentSite = new Sites();
			RefreshFiles();
			if (cmbSiteFiles.get_Items().get_Count() > 0)
			{
				((ListControl)cmbSiteFiles).set_SelectedIndex(0);
				return;
			}
			Button obj = cmdImport;
			bool enabled;
			((Control)cmdOpen).set_Enabled(enabled = false);
			((Control)obj).set_Enabled(enabled);
		}

		private void RefreshList(int SelectedLine)
		{
			int count = AllSites.Count;
			lstSiteNames.get_Items().Clear();
			AllSites.Sort();
			for (int i = 0; i < count; i++)
			{
				lstSiteNames.get_Items().Add((object)(AllSites[i].Name.PadRight(32) + "  " + Utilities.DEGtoDMS(AllSites[i].Longitude, 4, 0, MinutesOnly: true) + "  " + Utilities.DEGtoDMS(AllSites[i].Latitude, 4, 0, MinutesOnly: true)));
			}
			if (SelectedLine == count)
			{
				SelectedLine--;
			}
			if (SelectedLine >= 0 && SelectedLine < count)
			{
				((ListControl)lstSiteNames).set_SelectedIndex(SelectedLine);
			}
		}

		private void RefreshFiles()
		{
			cmbSiteFiles.get_Items().Clear();
			((ListControl)cmbSiteFiles).set_SelectedIndex(-1);
			((Control)cmbSiteFiles).set_Text("");
			if (!chkUseXML.get_Checked())
			{
				string[] files = Directory.GetFiles(AppPath + "\\Sites\\", "*.site");
				foreach (string path in files)
				{
					cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path));
				}
			}
			else
			{
				string[] files = Directory.GetFiles(AppPath + "\\Sites\\", "*.xml");
				foreach (string path2 in files)
				{
					cmbSiteFiles.get_Items().Add((object)Path.GetFileName(path2));
				}
			}
			if (cmbSiteFiles.get_Items().get_Count() > 0)
			{
				((ListControl)cmbSiteFiles).set_SelectedIndex(0);
			}
		}

		private void optDMS_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDMM_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void optDDD_CheckedChanged(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				DMSformatVisibility();
				UpdatingData = false;
			}
		}

		private void DMSformatVisibility()
		{
			((Control)panelDMS).set_Visible(optDMS.get_Checked());
			((Control)panelDMM).set_Visible(optDMM.get_Checked());
			((Control)panelDDD).set_Visible(optDDD.get_Checked());
		}

		private void txtLongD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongS_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatS_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongDdm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLongMdm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatDdm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatMdm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLongitude_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLatitude_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(2);
				UpdatingData = false;
			}
		}

		private void UpdateLongitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLongD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLongS).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLongDdm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDdm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMdm).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLongitude).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongitude).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				break;
			}
			double num = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num = 0.0 - num;
			}
			string text = Utilities.DEGtoDMS(num, 4, 1, MinutesOnly: false);
			((Control)txtLongD).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongM).set_Text(text.Substring(5, 2).Replace(" ", ""));
			((Control)txtLongS).set_Text(text.Substring(8).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 4, 3, MinutesOnly: true);
			((Control)txtLongDdm).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMdm).set_Text(text.Substring(5).Replace(" ", ""));
			((Control)txtLongitude).set_Text(string.Format("{0,2:F6}", num));
		}

		private void UpdateLatitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLatD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatD).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLatS).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLatDdm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDdm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMdm).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLatitude).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatitude).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				break;
			}
			double num = result + result2 / 60.0 + result3 / 3600.0;
			if (flag)
			{
				num = 0.0 - num;
			}
			string text = Utilities.DEGtoDMS(num, 3, 1, MinutesOnly: false);
			((Control)txtLatD).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatM).set_Text(text.Substring(4, 2).Replace(" ", ""));
			((Control)txtLatS).set_Text(text.Substring(7).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 3, 3, MinutesOnly: true);
			((Control)txtLatDdm).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMdm).set_Text(text.Substring(4).Replace(" ", ""));
			((Control)txtLatitude).set_Text(string.Format("{0,2:F6}", num));
		}

		private void optMeters_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		private void optFeet_CheckedChanged(object sender, EventArgs e)
		{
			AltitudeVisibility();
		}

		private void AltitudeVisibility()
		{
			((Control)txtAlt_m).set_Visible(optMeters.get_Checked());
			((Control)txtAlt_ft).set_Visible(!((Control)txtAlt_m).get_Visible());
		}

		private void txtAlt_m_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_m).get_Text(), out var result))
				{
					result = 0.0;
				}
				((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result));
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result / 0.3048));
				UpdatingData = false;
			}
		}

		private void txtAlt_ft_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				if (!double.TryParse(((Control)txtAlt_ft).get_Text(), out var result))
				{
					result = 0.0;
				}
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", result));
				((Control)txtAlt_m).set_Text(string.Format("{0,1:F0}", result * 0.3048));
				UpdatingData = false;
			}
		}

		private void cmdClear_Click(object sender, EventArgs e)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Invalid comparison between Unknown and I4
			if (!UnsavedChanges || (int)MessageBox.Show("Changes have not been saved.\r\n\r\nDo you want to continue with the Clear?", "Check unsaved changes", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				FileName = "";
				AllSites.Clear();
				RefreshList(0);
				((Control)cmdSave).set_Enabled(false);
				((Control)cmdSave).set_Text("Save\r\n[file name]");
				UnsavedChanges = false;
				Sites.SortField = Settings.Default.Sites_EditorSortOrder;
			}
		}

		private void lstSiteNames_SelectedIndexChanged(object sender, EventArgs e)
		{
			if (((ListControl)lstSiteNames).get_SelectedIndex() >= 0)
			{
				CurrentSite = AllSites[((ListControl)lstSiteNames).get_SelectedIndex()];
				((Control)txtLongitude).set_Text(CurrentSite.Longitude.ToString());
				((Control)txtLatitude).set_Text(CurrentSite.Latitude.ToString());
				((Control)txtAlt_m).set_Text(CurrentSite.Altitude.ToString());
				((Control)txtAlt_ft).set_Text(string.Format("{0,1:F0}", CurrentSite.Altitude / 0.3048));
				UpdatingData = true;
				UpdateLongitudes(2);
				UpdateLatitudes(2);
				UpdatingData = false;
				updnAperture.set_Value((decimal)CurrentSite.ApertureCM);
				updnMagCorrn.set_Value((decimal)CurrentSite.MagCorrection);
				updnGrazeDist.set_Value((decimal)CurrentSite.GrazeTravelDist);
				updnTimeZone.set_Value((decimal)CurrentSite.TimeZone);
				((Control)txtName).set_Text(CurrentSite.Name);
				((Control)txtShortName).set_Text(CurrentSite.ShortName);
				((ListControl)cmbSitePlot).set_SelectedIndex(CurrentSite.PlotOnMap);
				((ListControl)cmbGoogle).set_SelectedIndex(CurrentSite.PlotInGoogle);
			}
		}

		private void lstSiteNames_MouseDoubleClick(object sender, MouseEventArgs e)
		{
			lstSiteNames_SelectedIndexChanged(sender, (EventArgs)(object)e);
			cmdEdit_Click(sender, (EventArgs)(object)e);
		}

		private void cmdEdit_Click(object sender, EventArgs e)
		{
			if (lstSiteNames.get_Items().get_Count() >= 1)
			{
				SetControlVisibility(State: true);
				Edit = true;
				DontClose = true;
			}
		}

		private void SetControlVisibility(bool State)
		{
			TextBox obj = txtName;
			bool enabled;
			((Control)txtShortName).set_Enabled(enabled = State);
			((Control)obj).set_Enabled(enabled);
			TextBox obj2 = txtLongD;
			TextBox obj3 = txtLongM;
			TextBox obj4 = txtLongS;
			TextBox obj5 = txtLongDdm;
			TextBox obj6 = txtLongMdm;
			bool flag;
			((Control)txtLongitude).set_Enabled(flag = State);
			bool flag2;
			((Control)obj6).set_Enabled(flag2 = flag);
			bool flag3;
			((Control)obj5).set_Enabled(flag3 = flag2);
			bool flag4;
			((Control)obj4).set_Enabled(flag4 = flag3);
			((Control)obj3).set_Enabled(enabled = flag4);
			((Control)obj2).set_Enabled(enabled);
			TextBox obj7 = txtLatD;
			TextBox obj8 = txtLatM;
			TextBox obj9 = txtLatS;
			TextBox obj10 = txtLatDdm;
			TextBox obj11 = txtLatMdm;
			((Control)txtLatitude).set_Enabled(flag = State);
			((Control)obj11).set_Enabled(flag2 = flag);
			((Control)obj10).set_Enabled(flag3 = flag2);
			((Control)obj9).set_Enabled(flag4 = flag3);
			((Control)obj8).set_Enabled(enabled = flag4);
			((Control)obj7).set_Enabled(enabled);
			TextBox obj12 = txtAlt_m;
			((Control)txtAlt_ft).set_Enabled(enabled = State);
			((Control)obj12).set_Enabled(enabled);
			NumericUpDown obj13 = updnTimeZone;
			((Control)updnAperture).set_Enabled(enabled = State);
			((Control)obj13).set_Enabled(enabled);
			NumericUpDown obj14 = updnGrazeDist;
			NumericUpDown obj15 = updnMagCorrn;
			ComboBox obj16 = cmbSitePlot;
			ComboBox obj17 = cmbGoogle;
			Button obj18 = cmdCancel;
			((Control)cmdOK).set_Enabled(flag = State);
			((Control)obj18).set_Enabled(flag2 = flag);
			((Control)obj17).set_Enabled(flag3 = flag2);
			((Control)obj16).set_Enabled(flag4 = flag3);
			((Control)obj15).set_Enabled(enabled = flag4);
			((Control)obj14).set_Enabled(enabled);
			((Control)grpOpen).set_Visible(!State);
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			SetControlVisibility(State: true);
			Edit = false;
			DontClose = true;
		}

		private void cmdCancel_Click(object sender, EventArgs e)
		{
			lstSiteNames_SelectedIndexChanged(sender, e);
			SetControlVisibility(State: false);
			DontClose = false;
		}

		private void cmdOK_Click(object sender, EventArgs e)
		{
			double result;
			if (Edit)
			{
				if (!double.TryParse(((Control)txtLongitude).get_Text(), out result))
				{
					result = 0.0;
				}
				CurrentSite.Longitude = result;
				if (!double.TryParse(((Control)txtLatitude).get_Text(), out result))
				{
					result = 0.0;
				}
				CurrentSite.Latitude = result;
				if (!double.TryParse(((Control)txtAlt_m).get_Text(), out result))
				{
					result = 0.0;
				}
				CurrentSite.Altitude = result;
				CurrentSite.ApertureCM = (float)updnAperture.get_Value();
				CurrentSite.MagCorrection = (float)updnMagCorrn.get_Value();
				CurrentSite.GrazeTravelDist = (float)updnGrazeDist.get_Value();
				CurrentSite.TimeZone = (float)updnTimeZone.get_Value();
				CurrentSite.Name = ((Control)txtName).get_Text();
				CurrentSite.ShortName = ((Control)txtShortName).get_Text();
				CurrentSite.PlotOnMap = ((ListControl)cmbSitePlot).get_SelectedIndex();
				CurrentSite.PlotInGoogle = ((ListControl)cmbGoogle).get_SelectedIndex();
				RefreshList(((ListControl)lstSiteNames).get_SelectedIndex());
			}
			else
			{
				Sites sites = new Sites();
				if (!double.TryParse(((Control)txtLongitude).get_Text(), out result))
				{
					result = 0.0;
				}
				sites.Longitude = result;
				if (!double.TryParse(((Control)txtLatitude).get_Text(), out result))
				{
					result = 0.0;
				}
				sites.Latitude = result;
				if (!double.TryParse(((Control)txtAlt_m).get_Text(), out result))
				{
					result = 0.0;
				}
				sites.Altitude = result;
				sites.ApertureCM = (float)updnAperture.get_Value();
				sites.MagCorrection = (float)updnMagCorrn.get_Value();
				sites.GrazeTravelDist = (float)updnGrazeDist.get_Value();
				sites.TimeZone = (float)updnTimeZone.get_Value();
				sites.Name = ((Control)txtName).get_Text();
				sites.ShortName = ((Control)txtShortName).get_Text();
				sites.PlotOnMap = ((ListControl)cmbSitePlot).get_SelectedIndex();
				sites.PlotInGoogle = ((ListControl)cmbGoogle).get_SelectedIndex();
				AllSites.Add(sites);
				RefreshList(AllSites.Count - 1);
			}
			SetControlVisibility(State: false);
			UnsavedChanges = true;
			DontClose = false;
		}

		private void txtShortName_KeyUp(object sender, KeyEventArgs e)
		{
			((Control)lblShortChars).set_Text((9 - ((Control)txtShortName).get_Text().Length).ToString());
		}

		private void txtShortName_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtShortName).set_SelectionStart(0);
			((TextBoxBase)txtShortName).set_SelectionLength(((Control)txtShortName).get_Text().Length);
			((Control)lblShortChars).set_Text((9 - ((Control)txtShortName).get_Text().Length).ToString());
		}

		private void txtName_Enter(object sender, EventArgs e)
		{
			((TextBoxBase)txtName).set_SelectionStart(0);
			((TextBoxBase)txtName).set_SelectionLength(((Control)txtName).get_Text().Length);
			((Control)lblLongChar).set_Text((32 - ((Control)txtName).get_Text().Length).ToString());
		}

		private void txtName_KeyUp(object sender, KeyEventArgs e)
		{
			((Control)lblLongChar).set_Text((32 - ((Control)txtName).get_Text().Length).ToString());
		}

		private void cmdSortName_Click(object sender, EventArgs e)
		{
			Sites.SortField = 0;
			AllSites.Sort();
			RefreshList(0);
		}

		private void cmdSortByLongitude_Click(object sender, EventArgs e)
		{
			Sites.SortField = 1;
			AllSites.Sort();
			RefreshList(0);
		}

		private void cmdSortByLatitude_Click(object sender, EventArgs e)
		{
			Sites.SortField = 2;
			AllSites.Sort();
			RefreshList(0);
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_004f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0055: Invalid comparison between Unknown and I4
			if (lstSiteNames.get_Items().get_Count() < 1)
			{
				return;
			}
			int selectedIndex = ((ListControl)lstSiteNames).get_SelectedIndex();
			if (selectedIndex >= 0)
			{
				string text = lstSiteNames.get_Items().get_Item(selectedIndex).ToString();
				if ((int)MessageBox.Show("Do you want to delete \r\n\r\n" + text, "Confirm deletion", (MessageBoxButtons)1, (MessageBoxIcon)48) == 1)
				{
					AllSites.RemoveAt(selectedIndex);
					RefreshList(selectedIndex);
				}
				UnsavedChanges = true;
			}
		}

		private void cmdOpen_Click(object sender, EventArgs e)
		{
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_002b: Invalid comparison between Unknown and I4
			if (AllSites.Count > 0 && (int)MessageBox.Show("All listed sites will be cleared.\r\n\r\n Do you want to continue?", "Confirm deletion", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				return;
			}
			AllSites.Clear();
			if (!chkUseXML.get_Checked())
			{
				FileName = AppPath + "\\Sites\\" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex());
				StreamReader streamReader = new StreamReader(FileName);
				do
				{
					S = new Sites();
					string inLine = streamReader.ReadLine();
					S.Read_SiteFile(inLine);
					AllSites.Add(S);
				}
				while (!streamReader.EndOfStream);
				streamReader.Close();
			}
			AllSites.Sort();
			RefreshList(0);
			((Control)cmdSave).set_Enabled(true);
			((Control)cmdSave).set_Text("Save \r\n" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()));
			UnsavedChanges = false;
			((Control)lstSiteNames).Focus();
		}

		private void cmdSaveAs_Click(object sender, EventArgs e)
		{
			//IL_0000: Unknown result type (might be due to invalid IL or missing references)
			//IL_0006: Expected O, but got Unknown
			//IL_0062: Unknown result type (might be due to invalid IL or missing references)
			//IL_0068: Invalid comparison between Unknown and I4
			SaveFileDialog val = new SaveFileDialog();
			((FileDialog)val).set_Title("Specify site file name");
			((FileDialog)val).set_FilterIndex(1);
			((FileDialog)val).set_AddExtension(true);
			val.set_OverwritePrompt(true);
			((FileDialog)val).set_InitialDirectory(AppPath + "\\Sites\\");
			if (!chkUseXML.get_Checked())
			{
				((FileDialog)val).set_Filter("site file (*.site)|*.site");
			}
			else
			{
				((FileDialog)val).set_Filter("XML site file (*.xml)|*.xml");
			}
			if ((int)((CommonDialog)val).ShowDialog() == 1)
			{
				if (!chkUseXML.get_Checked())
				{
					FileName = ((FileDialog)val).get_FileName();
				}
				else
				{
					XMLFileName = ((FileDialog)val).get_FileName();
				}
				Save();
				Button obj = cmdSave;
				bool enabled;
				((Control)cmdOpen).set_Enabled(enabled = true);
				((Control)obj).set_Enabled(enabled);
				RefreshFiles();
				if (cmbSiteFiles.get_Items().get_Count() > 0)
				{
					((ListControl)cmbSiteFiles).set_SelectedIndex(0);
				}
				((Control)cmdSave).set_Text("Save \r\n" + Path.GetFileName(FileName));
			}
		}

		private void cmdSave_Click(object sender, EventArgs e)
		{
			Save();
		}

		private void Save()
		{
			if (!chkUseXML.get_Checked())
			{
				StreamWriter streamWriter = new StreamWriter(FileName);
				for (int i = 0; i < AllSites.Count; i++)
				{
					streamWriter.WriteLine(AllSites[i].ToString());
				}
				streamWriter.Close();
				UnsavedChanges = false;
			}
			else
			{
				XmlRootAttribute xmlRootAttribute = new XmlRootAttribute();
				xmlRootAttribute.ElementName = "Sites";
				xmlRootAttribute.IsNullable = true;
				XmlSerializer xmlSerializer = new XmlSerializer(typeof(List<Sites>), xmlRootAttribute);
				TextWriter textWriter = new StreamWriter(XMLFileName);
				xmlSerializer.Serialize(textWriter, AllSites);
			}
		}

		private void cmdImport_Click(object sender, EventArgs e)
		{
			StreamReader streamReader = new StreamReader(AppPath + "\\Sites\\" + cmbSiteFiles.get_Items().get_Item(((ListControl)cmbSiteFiles).get_SelectedIndex()));
			do
			{
				S = new Sites();
				string inLine = streamReader.ReadLine();
				S.Read_SiteFile(inLine);
				AllSites.Add(S);
			}
			while (!streamReader.EndOfStream);
			streamReader.Close();
			AllSites.Sort();
			RefreshList(0);
			UnsavedChanges = true;
		}

		private void SiteEditor_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_0030: Unknown result type (might be due to invalid IL or missing references)
			//IL_0036: Invalid comparison between Unknown and I4
			if (DontClose)
			{
				((CancelEventArgs)(object)e).Cancel = true;
			}
			else if (UnsavedChanges && (int)MessageBox.Show("Changes have not been saved.\r\n\r\nDo you want to continue with the closing the form?", "Check unsaved changes", (MessageBoxButtons)4, (MessageBoxIcon)48, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) == 7)
			{
				((CancelEventArgs)(object)e).Cancel = true;
				return;
			}
			((Component)this).Dispose();
		}

		private void chkUseXML_CheckedChanged(object sender, EventArgs e)
		{
			RefreshFiles();
		}

		private void exitToolStripMenuItem_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
		}

		private void helpToolStripMenuItem_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Site editor");
		}

		private void txtName_Leave(object sender, EventArgs e)
		{
			//IL_0185: Unknown result type (might be due to invalid IL or missing references)
			string text = "";
			if (((Control)txtName).get_Text().Contains("/"))
			{
				text += "/ ";
			}
			if (((Control)txtName).get_Text().Contains("\\"))
			{
				text += "\\ ";
			}
			if (((Control)txtName).get_Text().Contains(":"))
			{
				text += ": ";
			}
			if (((Control)txtName).get_Text().Contains("*"))
			{
				text += "* ";
			}
			if (((Control)txtName).get_Text().Contains("#"))
			{
				text += "# ";
			}
			if (((Control)txtName).get_Text().Contains("?"))
			{
				text += "? ";
			}
			if (((Control)txtName).get_Text().Contains("\""))
			{
				text += "\" ";
			}
			if (((Control)txtName).get_Text().Contains("<"))
			{
				text += "< ";
			}
			if (((Control)txtName).get_Text().Contains(">"))
			{
				text += "> ";
			}
			if (((Control)txtName).get_Text().Contains("|"))
			{
				text += "| ";
			}
			if (text.Length > 0)
			{
				MessageBox.Show("Site name contains the invalid character(s) " + text + "\r\nThese characters must be removed", "Invalid character", (MessageBoxButtons)0, (MessageBoxIcon)16);
				((Control)txtName).Focus();
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
			//IL_04c9: Unknown result type (might be due to invalid IL or missing references)
			//IL_04d3: Expected O, but got Unknown
			//IL_07bb: Unknown result type (might be due to invalid IL or missing references)
			//IL_07c5: Expected O, but got Unknown
			//IL_0ad2: Unknown result type (might be due to invalid IL or missing references)
			//IL_0adc: Expected O, but got Unknown
			//IL_2a97: Unknown result type (might be due to invalid IL or missing references)
			//IL_2aa1: Expected O, but got Unknown
			//IL_339c: Unknown result type (might be due to invalid IL or missing references)
			//IL_33a6: Expected O, but got Unknown
			//IL_3414: Unknown result type (might be due to invalid IL or missing references)
			//IL_341e: Expected O, but got Unknown
			lstSiteNames = new ListBox();
			label1 = new Label();
			txtLongitude = new TextBox();
			txtLatitude = new TextBox();
			txtAlt_m = new TextBox();
			txtName = new TextBox();
			updnAperture = new NumericUpDown();
			updnMagCorrn = new NumericUpDown();
			updnGrazeDist = new NumericUpDown();
			txtShortName = new TextBox();
			cmbSitePlot = new ComboBox();
			label39 = new Label();
			label36 = new Label();
			label38 = new Label();
			label37 = new Label();
			txtLongS = new TextBox();
			txtLongM = new TextBox();
			txtLongD = new TextBox();
			txtLatS = new TextBox();
			txtLatM = new TextBox();
			txtLatD = new TextBox();
			label20 = new Label();
			label24 = new Label();
			label25 = new Label();
			label2 = new Label();
			label3 = new Label();
			label4 = new Label();
			label5 = new Label();
			label6 = new Label();
			cmdClear = new Button();
			grpEdit = new GroupBox();
			txtAlt_ft = new TextBox();
			panel2 = new Panel();
			optFeet = new RadioButton();
			optMeters = new RadioButton();
			panel1 = new Panel();
			optDMM = new RadioButton();
			optDDD = new RadioButton();
			optDMS = new RadioButton();
			panelDDD = new Panel();
			label26 = new Label();
			panelDMM = new Panel();
			txtLatMdm = new TextBox();
			txtLatDdm = new TextBox();
			txtLongMdm = new TextBox();
			txtLongDdm = new TextBox();
			label21 = new Label();
			label22 = new Label();
			panelDMS = new Panel();
			label19 = new Label();
			cmbGoogle = new ComboBox();
			label18 = new Label();
			label17 = new Label();
			label15 = new Label();
			updnTimeZone = new NumericUpDown();
			label7 = new Label();
			lblLongChar = new Label();
			lblShortChars = new Label();
			cmdCancel = new Button();
			cmdOK = new Button();
			cmdEdit = new Button();
			cmdAdd = new Button();
			cmdDelete = new Button();
			pnlEdit = new Panel();
			grpOpen = new GroupBox();
			chkUseXML = new CheckBox();
			cmdSaveAs = new Button();
			cmdSave = new Button();
			cmdImport = new Button();
			cmdOpen = new Button();
			label14 = new Label();
			cmbSiteFiles = new ComboBox();
			cmdSortName = new Button();
			cmdSortByLongitude = new Button();
			cmdSortByLatitude = new Button();
			label12 = new Label();
			label13 = new Label();
			panelList = new Panel();
			label16 = new Label();
			menuStrip1 = new MenuStrip();
			helpToolStripMenuItem = new ToolStripMenuItem();
			exitToolStripMenuItem = new ToolStripMenuItem();
			((ISupportInitialize)updnAperture).BeginInit();
			((ISupportInitialize)updnMagCorrn).BeginInit();
			((ISupportInitialize)updnGrazeDist).BeginInit();
			((Control)grpEdit).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)panelDDD).SuspendLayout();
			((Control)panelDMM).SuspendLayout();
			((Control)panelDMS).SuspendLayout();
			((ISupportInitialize)updnTimeZone).BeginInit();
			((Control)pnlEdit).SuspendLayout();
			((Control)grpOpen).SuspendLayout();
			((Control)panelList).SuspendLayout();
			((Control)menuStrip1).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)lstSiteNames).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSiteNames).set_FormattingEnabled(true);
			lstSiteNames.set_ItemHeight(14);
			((Control)lstSiteNames).set_Location(new Point(4, 22));
			((Control)lstSiteNames).set_Name("lstSiteNames");
			((Control)lstSiteNames).set_Size(new Size(393, 200));
			((Control)lstSiteNames).set_TabIndex(4);
			lstSiteNames.add_SelectedIndexChanged((EventHandler)lstSiteNames_SelectedIndexChanged);
			((Control)lstSiteNames).add_MouseDoubleClick(new MouseEventHandler(lstSiteNames_MouseDoubleClick));
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Location(new Point(17, 6));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(54, 13));
			((Control)label1).set_TabIndex(0);
			((Control)label1).set_Text("Site name");
			((Control)txtLongitude).set_Enabled(false);
			((Control)txtLongitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongitude).set_Location(new Point(2, 12));
			((Control)txtLongitude).set_Name("txtLongitude");
			((Control)txtLongitude).set_Size(new Size(82, 20));
			((Control)txtLongitude).set_TabIndex(1);
			((Control)txtLongitude).set_Text("0");
			txtLongitude.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLongitude).add_Leave((EventHandler)txtLongitude_Leave);
			((Control)txtLatitude).set_Enabled(false);
			((Control)txtLatitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatitude).set_Location(new Point(2, 38));
			((Control)txtLatitude).set_Name("txtLatitude");
			((Control)txtLatitude).set_Size(new Size(82, 20));
			((Control)txtLatitude).set_TabIndex(2);
			((Control)txtLatitude).set_Text("0");
			txtLatitude.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLatitude).add_Leave((EventHandler)txtLatitude_Leave);
			((Control)txtAlt_m).set_Enabled(false);
			((Control)txtAlt_m).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_m).set_Location(new Point(204, 135));
			((Control)txtAlt_m).set_Name("txtAlt_m");
			((Control)txtAlt_m).set_Size(new Size(53, 20));
			((Control)txtAlt_m).set_TabIndex(15);
			((Control)txtAlt_m).set_Text("0");
			txtAlt_m.set_TextAlign((HorizontalAlignment)2);
			((Control)txtAlt_m).add_Leave((EventHandler)txtAlt_m_Leave);
			((Control)txtName).set_Enabled(false);
			((Control)txtName).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtName).set_Location(new Point(204, 22));
			((TextBoxBase)txtName).set_MaxLength(32);
			((Control)txtName).set_Name("txtName");
			((Control)txtName).set_Size(new Size(239, 20));
			((Control)txtName).set_TabIndex(1);
			((Control)txtName).add_Enter((EventHandler)txtName_Enter);
			((Control)txtName).add_KeyUp(new KeyEventHandler(txtName_KeyUp));
			((Control)txtName).add_Leave((EventHandler)txtName_Leave);
			((Control)updnAperture).set_Enabled(false);
			((Control)updnAperture).set_Location(new Point(204, 187));
			updnAperture.set_Maximum(new decimal(new int[4] { 500, 0, 0, 0 }));
			updnAperture.set_Minimum(new decimal(new int[4] { 5, 0, 0, 0 }));
			((Control)updnAperture).set_Name("updnAperture");
			((Control)updnAperture).set_Size(new Size(52, 20));
			((Control)updnAperture).set_TabIndex(20);
			((UpDownBase)updnAperture).set_TextAlign((HorizontalAlignment)2);
			updnAperture.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			updnMagCorrn.set_DecimalPlaces(1);
			((Control)updnMagCorrn).set_Enabled(false);
			updnMagCorrn.set_Increment(new decimal(new int[4] { 2, 0, 0, 65536 }));
			((Control)updnMagCorrn).set_Location(new Point(204, 213));
			updnMagCorrn.set_Maximum(new decimal(new int[4] { 2, 0, 0, 0 }));
			updnMagCorrn.set_Minimum(new decimal(new int[4] { 1, 0, 0, -2147483648 }));
			((Control)updnMagCorrn).set_Name("updnMagCorrn");
			((Control)updnMagCorrn).set_Size(new Size(50, 20));
			((Control)updnMagCorrn).set_TabIndex(22);
			((UpDownBase)updnMagCorrn).set_TextAlign((HorizontalAlignment)2);
			((Control)updnGrazeDist).set_Enabled(false);
			updnGrazeDist.set_Increment(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGrazeDist).set_Location(new Point(204, 239));
			updnGrazeDist.set_Maximum(new decimal(new int[4] { 1000, 0, 0, 0 }));
			updnGrazeDist.set_Minimum(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)updnGrazeDist).set_Name("updnGrazeDist");
			((Control)updnGrazeDist).set_Size(new Size(50, 20));
			((Control)updnGrazeDist).set_TabIndex(25);
			((UpDownBase)updnGrazeDist).set_TextAlign((HorizontalAlignment)2);
			updnGrazeDist.set_Value(new decimal(new int[4] { 10, 0, 0, 0 }));
			((Control)txtShortName).set_Enabled(false);
			((Control)txtShortName).set_Font(new Font("Courier New", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtShortName).set_Location(new Point(204, 48));
			((TextBoxBase)txtShortName).set_MaxLength(9);
			((Control)txtShortName).set_Name("txtShortName");
			((Control)txtShortName).set_Size(new Size(75, 20));
			((Control)txtShortName).set_TabIndex(6);
			((Control)txtShortName).add_Enter((EventHandler)txtShortName_Enter);
			((Control)txtShortName).add_KeyUp(new KeyEventHandler(txtShortName_KeyUp));
			((Control)cmbSitePlot).set_Enabled(false);
			((ListControl)cmbSitePlot).set_FormattingEnabled(true);
			cmbSitePlot.get_Items().AddRange(new object[5] { "Never", "only on Detailed maps", "on Medium maps", "on All maps", "mobile site" });
			((Control)cmbSitePlot).set_Location(new Point(204, 265));
			((Control)cmbSitePlot).set_Name("cmbSitePlot");
			((Control)cmbSitePlot).set_Size(new Size(135, 21));
			((Control)cmbSitePlot).set_TabIndex(27);
			((Control)label39).set_AutoSize(true);
			((Control)label39).set_Location(new Point(72, 268));
			((Control)label39).set_Name("label39");
			((Control)label39).set_Size(new Size(115, 13));
			((Control)label39).set_TabIndex(26);
			((Control)label39).set_Text("Include in Occult maps");
			((Control)label36).set_AutoSize(true);
			((Control)label36).set_Location(new Point(9, 241));
			((Control)label36).set_Name("label36");
			((Control)label36).set_Size(new Size(178, 13));
			((Control)label36).set_TabIndex(24);
			((Control)label36).set_Text("Travel distance for lunar grazes (km)");
			label36.set_TextAlign(ContentAlignment.TopCenter);
			((Control)label38).set_AutoSize(true);
			((Control)label38).set_Location(new Point(34, 215));
			((Control)label38).set_Name("label38");
			((Control)label38).set_Size(new Size(153, 13));
			((Control)label38).set_TabIndex(21);
			((Control)label38).set_Text("Correction to limiting magnitude");
			((Control)label37).set_AutoSize(true);
			((Control)label37).set_Location(new Point(65, 189));
			((Control)label37).set_Name("label37");
			((Control)label37).set_Size(new Size(122, 13));
			((Control)label37).set_TabIndex(19);
			((Control)label37).set_Text("Telescope aperture (cm)");
			((Control)txtLongS).set_Enabled(false);
			((Control)txtLongS).set_Location(new Point(68, 12));
			((Control)txtLongS).set_Name("txtLongS");
			((Control)txtLongS).set_Size(new Size(31, 20));
			((Control)txtLongS).set_TabIndex(5);
			((Control)txtLongS).set_Text("0");
			txtLongS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLongS).add_Leave((EventHandler)txtLongS_Leave);
			((Control)txtLongM).set_Enabled(false);
			((Control)txtLongM).set_Location(new Point(36, 12));
			((Control)txtLongM).set_Name("txtLongM");
			((Control)txtLongM).set_Size(new Size(23, 20));
			((Control)txtLongM).set_TabIndex(4);
			((Control)txtLongM).set_Text("0");
			txtLongM.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongM).add_Leave((EventHandler)txtLongM_Leave);
			((Control)txtLongD).set_Enabled(false);
			((Control)txtLongD).set_Location(new Point(2, 12));
			((Control)txtLongD).set_Name("txtLongD");
			((Control)txtLongD).set_Size(new Size(28, 20));
			((Control)txtLongD).set_TabIndex(3);
			((Control)txtLongD).set_Text("0");
			txtLongD.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongD).add_Leave((EventHandler)txtLongD_Leave);
			((Control)txtLatS).set_Enabled(false);
			((Control)txtLatS).set_Location(new Point(68, 38));
			((Control)txtLatS).set_Name("txtLatS");
			((Control)txtLatS).set_Size(new Size(31, 20));
			((Control)txtLatS).set_TabIndex(8);
			((Control)txtLatS).set_Text("0");
			txtLatS.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLatS).add_Leave((EventHandler)txtLatS_Leave);
			((Control)txtLatM).set_Enabled(false);
			((Control)txtLatM).set_Location(new Point(36, 38));
			((Control)txtLatM).set_Name("txtLatM");
			((Control)txtLatM).set_Size(new Size(23, 20));
			((Control)txtLatM).set_TabIndex(7);
			((Control)txtLatM).set_Text("0");
			txtLatM.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLatM).add_Leave((EventHandler)txtLatM_Leave);
			((Control)txtLatD).set_Enabled(false);
			((Control)txtLatD).set_Location(new Point(2, 38));
			((Control)txtLatD).set_Name("txtLatD");
			((Control)txtLatD).set_Size(new Size(28, 20));
			((Control)txtLatD).set_TabIndex(6);
			((Control)txtLatD).set_Text("0");
			txtLatD.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLatD).add_Leave((EventHandler)txtLatD_Leave);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Location(new Point(17, -2));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(13, 13));
			((Control)label20).set_TabIndex(0);
			((Control)label20).set_Text("o");
			((Control)label24).set_AutoSize(true);
			((Control)label24).set_Location(new Point(51, 1));
			((Control)label24).set_Name("label24");
			((Control)label24).set_Size(new Size(9, 13));
			((Control)label24).set_TabIndex(1);
			((Control)label24).set_Text("'");
			((Control)label25).set_AutoSize(true);
			((Control)label25).set_Location(new Point(81, 1));
			((Control)label25).set_Name("label25");
			((Control)label25).set_Size(new Size(12, 13));
			((Control)label25).set_TabIndex(2);
			((Control)label25).set_Text("\"");
			((Control)label2).set_AutoSize(true);
			((Control)label2).set_Location(new Point(109, 85));
			((Control)label2).set_Name("label2");
			((Control)label2).set_Size(new Size(78, 13));
			((Control)label2).set_TabIndex(9);
			((Control)label2).set_Text("East Longitude");
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Location(new Point(142, 111));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(45, 13));
			((Control)label3).set_TabIndex(10);
			((Control)label3).set_Text("Latitude");
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Location(new Point(128, 138));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(59, 13));
			((Control)label4).set_TabIndex(14);
			((Control)label4).set_Text("Altitude (m)");
			((Control)label5).set_AutoSize(true);
			((Control)label5).set_Location(new Point(133, 25));
			((Control)label5).set_Name("label5");
			((Control)label5).set_Size(new Size(54, 13));
			((Control)label5).set_TabIndex(0);
			((Control)label5).set_Text("Site name");
			((Control)label6).set_AutoSize(true);
			((Control)label6).set_Location(new Point(107, 51));
			((Control)label6).set_Name("label6");
			((Control)label6).set_Size(new Size(80, 13));
			((Control)label6).set_TabIndex(4);
			((Control)label6).set_Text("Short site name");
			((Control)cmdClear).set_Location(new Point(309, 228));
			((Control)cmdClear).set_Name("cmdClear");
			((Control)cmdClear).set_Size(new Size(74, 41));
			((Control)cmdClear).set_TabIndex(11);
			((Control)cmdClear).set_Text("Clear ALL");
			((ButtonBase)cmdClear).set_UseVisualStyleBackColor(true);
			((Control)cmdClear).add_Click((EventHandler)cmdClear_Click);
			((Control)grpEdit).set_Anchor((AnchorStyles)0);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtAlt_ft);
			((Control)grpEdit).get_Controls().Add((Control)(object)panel2);
			((Control)grpEdit).get_Controls().Add((Control)(object)panel1);
			((Control)grpEdit).get_Controls().Add((Control)(object)panelDDD);
			((Control)grpEdit).get_Controls().Add((Control)(object)panelDMM);
			((Control)grpEdit).get_Controls().Add((Control)(object)panelDMS);
			((Control)grpEdit).get_Controls().Add((Control)(object)label19);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmbGoogle);
			((Control)grpEdit).get_Controls().Add((Control)(object)label18);
			((Control)grpEdit).get_Controls().Add((Control)(object)label17);
			((Control)grpEdit).get_Controls().Add((Control)(object)label15);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnTimeZone);
			((Control)grpEdit).get_Controls().Add((Control)(object)label7);
			((Control)grpEdit).get_Controls().Add((Control)(object)lblLongChar);
			((Control)grpEdit).get_Controls().Add((Control)(object)lblShortChars);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdCancel);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmdOK);
			((Control)grpEdit).get_Controls().Add((Control)(object)label6);
			((Control)grpEdit).get_Controls().Add((Control)(object)label5);
			((Control)grpEdit).get_Controls().Add((Control)(object)label4);
			((Control)grpEdit).get_Controls().Add((Control)(object)label3);
			((Control)grpEdit).get_Controls().Add((Control)(object)label2);
			((Control)grpEdit).get_Controls().Add((Control)(object)label37);
			((Control)grpEdit).get_Controls().Add((Control)(object)label38);
			((Control)grpEdit).get_Controls().Add((Control)(object)label36);
			((Control)grpEdit).get_Controls().Add((Control)(object)cmbSitePlot);
			((Control)grpEdit).get_Controls().Add((Control)(object)label39);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtShortName);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnGrazeDist);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnMagCorrn);
			((Control)grpEdit).get_Controls().Add((Control)(object)updnAperture);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtName);
			((Control)grpEdit).get_Controls().Add((Control)(object)txtAlt_m);
			((Control)grpEdit).set_Location(new Point(8, 322));
			((Control)grpEdit).set_Name("grpEdit");
			((Control)grpEdit).set_Size(new Size(471, 365));
			((Control)grpEdit).set_TabIndex(1);
			grpEdit.set_TabStop(false);
			((Control)grpEdit).set_Text("Edit site details");
			((Control)txtAlt_ft).set_BackColor(SystemColors.Window);
			((Control)txtAlt_ft).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_ft).set_Location(new Point(204, 142));
			((Control)txtAlt_ft).set_Name("txtAlt_ft");
			((Control)txtAlt_ft).set_Size(new Size(53, 20));
			((Control)txtAlt_ft).set_TabIndex(16);
			((Control)txtAlt_ft).set_Text("0");
			txtAlt_ft.set_TextAlign((HorizontalAlignment)2);
			((Control)txtAlt_ft).set_Visible(false);
			((Control)txtAlt_ft).add_Leave((EventHandler)txtAlt_ft_Leave);
			((Control)panel2).get_Controls().Add((Control)(object)optFeet);
			((Control)panel2).get_Controls().Add((Control)(object)optMeters);
			((Control)panel2).set_Location(new Point(264, 129));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(53, 34));
			((Control)panel2).set_TabIndex(15);
			((Control)optFeet).set_AutoSize(true);
			((Control)optFeet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optFeet).set_Location(new Point(2, 16));
			((Control)optFeet).set_Name("optFeet");
			((Control)optFeet).set_Size(new Size(31, 17));
			((Control)optFeet).set_TabIndex(1);
			optFeet.set_TabStop(true);
			((Control)optFeet).set_Text("ft");
			((ButtonBase)optFeet).set_UseVisualStyleBackColor(true);
			optFeet.add_CheckedChanged((EventHandler)optFeet_CheckedChanged);
			((Control)optMeters).set_AutoSize(true);
			optMeters.set_Checked(true);
			((Control)optMeters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optMeters).set_Location(new Point(2, 1));
			((Control)optMeters).set_Name("optMeters");
			((Control)optMeters).set_Size(new Size(33, 17));
			((Control)optMeters).set_TabIndex(0);
			optMeters.set_TabStop(true);
			((Control)optMeters).set_Text("m");
			((ButtonBase)optMeters).set_UseVisualStyleBackColor(true);
			((Control)panel1).get_Controls().Add((Control)(object)optDMM);
			((Control)panel1).get_Controls().Add((Control)(object)optDDD);
			((Control)panel1).get_Controls().Add((Control)(object)optDMS);
			((Control)panel1).set_Location(new Point(308, 76));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(61, 56));
			((Control)panel1).set_TabIndex(8);
			((Control)optDMM).set_AutoSize(true);
			((Control)optDMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDMM).set_Location(new Point(3, 20));
			((Control)optDMM).set_Name("optDMM");
			((Control)optDMM).set_Size(new Size(50, 17));
			((Control)optDMM).set_TabIndex(1);
			optDMM.set_TabStop(true);
			((Control)optDMM).set_Text("dm.m");
			((ButtonBase)optDMM).set_UseVisualStyleBackColor(true);
			optDMM.add_CheckedChanged((EventHandler)optDMM_CheckedChanged);
			((Control)optDDD).set_AutoSize(true);
			((Control)optDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDDD).set_Location(new Point(3, 35));
			((Control)optDDD).set_Name("optDDD");
			((Control)optDDD).set_Size(new Size(46, 17));
			((Control)optDDD).set_TabIndex(2);
			optDDD.set_TabStop(true);
			((Control)optDDD).set_Text("d.dd");
			((ButtonBase)optDDD).set_UseVisualStyleBackColor(true);
			optDDD.add_CheckedChanged((EventHandler)optDDD_CheckedChanged);
			((Control)optDMS).set_AutoSize(true);
			optDMS.set_Checked(true);
			((Control)optDMS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)optDMS).set_Location(new Point(3, 5));
			((Control)optDMS).set_Name("optDMS");
			((Control)optDMS).set_Size(new Size(44, 17));
			((Control)optDMS).set_TabIndex(0);
			optDMS.set_TabStop(true);
			((Control)optDMS).set_Text("dms");
			((ButtonBase)optDMS).set_UseVisualStyleBackColor(true);
			optDMS.add_CheckedChanged((EventHandler)optDMS_CheckedChanged);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLatitude);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLongitude);
			((Control)panelDDD).get_Controls().Add((Control)(object)label26);
			((Control)panelDDD).set_Location(new Point(357, 246));
			((Control)panelDDD).set_Name("panelDDD");
			((Control)panelDDD).set_Size(new Size(107, 64));
			((Control)panelDDD).set_TabIndex(13);
			((Control)panelDDD).set_Visible(false);
			((Control)label26).set_AutoSize(true);
			((Control)label26).set_Location(new Point(17, -2));
			((Control)label26).set_Name("label26");
			((Control)label26).set_Size(new Size(13, 13));
			((Control)label26).set_TabIndex(0);
			((Control)label26).set_Text("o");
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatMdm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatDdm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongMdm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongDdm);
			((Control)panelDMM).get_Controls().Add((Control)(object)label21);
			((Control)panelDMM).get_Controls().Add((Control)(object)label22);
			((Control)panelDMM).set_Location(new Point(357, 143));
			((Control)panelDMM).set_Name("panelDMM");
			((Control)panelDMM).set_Size(new Size(107, 64));
			((Control)panelDMM).set_TabIndex(12);
			((Control)panelDMM).set_Visible(false);
			((Control)txtLatMdm).set_Enabled(false);
			((Control)txtLatMdm).set_Location(new Point(36, 38));
			((Control)txtLatMdm).set_Name("txtLatMdm");
			((Control)txtLatMdm).set_Size(new Size(44, 20));
			((Control)txtLatMdm).set_TabIndex(5);
			((Control)txtLatMdm).set_Text("0");
			txtLatMdm.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLatMdm).add_Leave((EventHandler)txtLatMdm_Leave);
			((Control)txtLatDdm).set_Enabled(false);
			((Control)txtLatDdm).set_Location(new Point(2, 38));
			((Control)txtLatDdm).set_Name("txtLatDdm");
			((Control)txtLatDdm).set_Size(new Size(28, 20));
			((Control)txtLatDdm).set_TabIndex(4);
			((Control)txtLatDdm).set_Text("0");
			txtLatDdm.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLatDdm).add_Leave((EventHandler)txtLatDdm_Leave);
			((Control)txtLongMdm).set_Enabled(false);
			((Control)txtLongMdm).set_Location(new Point(36, 12));
			((Control)txtLongMdm).set_Name("txtLongMdm");
			((Control)txtLongMdm).set_Size(new Size(44, 20));
			((Control)txtLongMdm).set_TabIndex(3);
			((Control)txtLongMdm).set_Text("0");
			txtLongMdm.set_TextAlign((HorizontalAlignment)2);
			((Control)txtLongMdm).add_Leave((EventHandler)txtLongMdm_Leave);
			((Control)txtLongDdm).set_Enabled(false);
			((Control)txtLongDdm).set_Location(new Point(2, 12));
			((Control)txtLongDdm).set_Name("txtLongDdm");
			((Control)txtLongDdm).set_Size(new Size(28, 20));
			((Control)txtLongDdm).set_TabIndex(2);
			((Control)txtLongDdm).set_Text("0");
			txtLongDdm.set_TextAlign((HorizontalAlignment)1);
			((Control)txtLongDdm).add_Leave((EventHandler)txtLongDdm_Leave);
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Location(new Point(17, -2));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(13, 13));
			((Control)label21).set_TabIndex(0);
			((Control)label21).set_Text("o");
			((Control)label22).set_AutoSize(true);
			((Control)label22).set_Location(new Point(51, 1));
			((Control)label22).set_Name("label22");
			((Control)label22).set_Size(new Size(9, 13));
			((Control)label22).set_TabIndex(1);
			((Control)label22).set_Text("'");
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatS);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatM);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatD);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongS);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongM);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongD);
			((Control)panelDMS).get_Controls().Add((Control)(object)label20);
			((Control)panelDMS).get_Controls().Add((Control)(object)label24);
			((Control)panelDMS).get_Controls().Add((Control)(object)label25);
			((Control)panelDMS).set_Location(new Point(202, 70));
			((Control)panelDMS).set_Name("panelDMS");
			((Control)panelDMS).set_Size(new Size(107, 64));
			((Control)panelDMS).set_TabIndex(11);
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 6.5f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(288, 9));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(156, 12));
			((Control)label19).set_TabIndex(2);
			((Control)label19).set_Text("Do not use any of   / \\  :  *  #  ?  \"  <  >  | \r\n");
			((Control)cmbGoogle).set_Enabled(false);
			((ListControl)cmbGoogle).set_FormattingEnabled(true);
			cmbGoogle.get_Items().AddRange(new object[3] { "never", "using low precision", "using full precision" });
			((Control)cmbGoogle).set_Location(new Point(203, 292));
			((Control)cmbGoogle).set_Name("cmbGoogle");
			((Control)cmbGoogle).set_Size(new Size(136, 21));
			((Control)cmbGoogle).set_TabIndex(29);
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Location(new Point(69, 295));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(118, 13));
			((Control)label18).set_TabIndex(28);
			((Control)label18).set_Text("Include in Google maps");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(268, 217));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(138, 12));
			((Control)label17).set_TabIndex(23);
			((Control)label17).set_Text("(for lunar occultation predictions)");
			((Control)label15).set_AutoSize(true);
			((Control)label15).set_Location(new Point(131, 163));
			((Control)label15).set_Name("label15");
			((Control)label15).set_Size(new Size(56, 13));
			((Control)label15).set_TabIndex(17);
			((Control)label15).set_Text("Time zone");
			updnTimeZone.set_DecimalPlaces(1);
			((Control)updnTimeZone).set_Enabled(false);
			updnTimeZone.set_Increment(new decimal(new int[4] { 5, 0, 0, 65536 }));
			((Control)updnTimeZone).set_Location(new Point(204, 161));
			updnTimeZone.set_Maximum(new decimal(new int[4] { 18, 0, 0, 0 }));
			updnTimeZone.set_Minimum(new decimal(new int[4] { 18, 0, 0, -2147483648 }));
			((Control)updnTimeZone).set_Name("updnTimeZone");
			((Control)updnTimeZone).set_Size(new Size(50, 20));
			((Control)updnTimeZone).set_TabIndex(18);
			((UpDownBase)updnTimeZone).set_TextAlign((HorizontalAlignment)2);
			((Control)label7).set_AutoSize(true);
			((Control)label7).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label7).set_Location(new Point(139, 62));
			((Control)label7).set_Name("label7");
			((Control)label7).set_Size(new Size(47, 12));
			((Control)label7).set_TabIndex(5);
			((Control)label7).set_Text("(for maps)");
			((Control)lblLongChar).set_AutoSize(true);
			((Control)lblLongChar).set_ForeColor(Color.Blue);
			((Control)lblLongChar).set_Location(new Point(445, 26));
			((Control)lblLongChar).set_Name("lblLongChar");
			((Control)lblLongChar).set_Size(new Size(19, 13));
			((Control)lblLongChar).set_TabIndex(3);
			((Control)lblLongChar).set_Text("32");
			((Control)lblShortChars).set_AutoSize(true);
			((Control)lblShortChars).set_ForeColor(Color.Blue);
			((Control)lblShortChars).set_Location(new Point(282, 52));
			((Control)lblShortChars).set_Name("lblShortChars");
			((Control)lblShortChars).set_Size(new Size(13, 13));
			((Control)lblShortChars).set_TabIndex(7);
			((Control)lblShortChars).set_Text("9");
			((Control)cmdCancel).set_Enabled(false);
			((Control)cmdCancel).set_Location(new Point(277, 326));
			((Control)cmdCancel).set_Name("cmdCancel");
			((Control)cmdCancel).set_Size(new Size(82, 31));
			((Control)cmdCancel).set_TabIndex(32);
			((Control)cmdCancel).set_Text("Cancel");
			((ButtonBase)cmdCancel).set_UseVisualStyleBackColor(true);
			((Control)cmdCancel).add_Click((EventHandler)cmdCancel_Click);
			((Control)cmdOK).set_Enabled(false);
			((Control)cmdOK).set_Location(new Point(105, 326));
			((Control)cmdOK).set_Name("cmdOK");
			((Control)cmdOK).set_Size(new Size(82, 31));
			((Control)cmdOK).set_TabIndex(30);
			((Control)cmdOK).set_Text("Accept");
			((ButtonBase)cmdOK).set_UseVisualStyleBackColor(true);
			((Control)cmdOK).add_Click((EventHandler)cmdOK_Click);
			((Control)cmdEdit).set_Location(new Point(12, 228));
			((Control)cmdEdit).set_Name("cmdEdit");
			((Control)cmdEdit).set_Size(new Size(74, 41));
			((Control)cmdEdit).set_TabIndex(8);
			((Control)cmdEdit).set_Text("Edit site");
			((ButtonBase)cmdEdit).set_UseVisualStyleBackColor(true);
			((Control)cmdEdit).add_Click((EventHandler)cmdEdit_Click);
			((Control)cmdAdd).set_Location(new Point(111, 228));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(74, 41));
			((Control)cmdAdd).set_TabIndex(9);
			((Control)cmdAdd).set_Text("Add new site");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(true);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)cmdDelete).set_Location(new Point(210, 228));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(74, 41));
			((Control)cmdDelete).set_TabIndex(10);
			((Control)cmdDelete).set_Text("Delete site");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(true);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)pnlEdit).set_Anchor((AnchorStyles)0);
			((Control)pnlEdit).get_Controls().Add((Control)(object)grpOpen);
			((Control)pnlEdit).set_Location(new Point(515, 27));
			((Control)pnlEdit).set_Name("pnlEdit");
			((Control)pnlEdit).set_Size(new Size(250, 279));
			((Control)pnlEdit).set_TabIndex(2);
			((Control)grpOpen).set_Anchor((AnchorStyles)1);
			((Control)grpOpen).get_Controls().Add((Control)(object)chkUseXML);
			((Control)grpOpen).get_Controls().Add((Control)(object)cmdSaveAs);
			((Control)grpOpen).get_Controls().Add((Control)(object)cmdSave);
			((Control)grpOpen).get_Controls().Add((Control)(object)cmdImport);
			((Control)grpOpen).get_Controls().Add((Control)(object)cmdOpen);
			((Control)grpOpen).get_Controls().Add((Control)(object)label14);
			((Control)grpOpen).get_Controls().Add((Control)(object)cmbSiteFiles);
			((Control)grpOpen).set_Location(new Point(6, 3));
			((Control)grpOpen).set_Name("grpOpen");
			((Control)grpOpen).set_Size(new Size(238, 468));
			((Control)grpOpen).set_TabIndex(0);
			grpOpen.set_TabStop(false);
			((Control)grpOpen).set_Text("Open/save site files");
			((Control)chkUseXML).set_AutoSize(true);
			chkUseXML.set_Checked(Settings.Default.UseXMLforSiteFiles);
			((Control)chkUseXML).get_DataBindings().Add(new Binding("Checked", (object)Settings.Default, "UseXMLforSiteFiles", true, (DataSourceUpdateMode)1));
			((Control)chkUseXML).set_Enabled(false);
			((Control)chkUseXML).set_Location(new Point(14, 185));
			((Control)chkUseXML).set_Name("chkUseXML");
			((Control)chkUseXML).set_Size(new Size(70, 17));
			((Control)chkUseXML).set_TabIndex(6);
			((Control)chkUseXML).set_Text("Use XML");
			((ButtonBase)chkUseXML).set_UseVisualStyleBackColor(true);
			chkUseXML.add_CheckedChanged((EventHandler)chkUseXML_CheckedChanged);
			((Control)cmdSaveAs).set_Location(new Point(138, 128));
			((Control)cmdSaveAs).set_Name("cmdSaveAs");
			((Control)cmdSaveAs).set_Size(new Size(89, 37));
			((Control)cmdSaveAs).set_TabIndex(5);
			((Control)cmdSaveAs).set_Text("Save as...");
			((ButtonBase)cmdSaveAs).set_UseVisualStyleBackColor(true);
			((Control)cmdSaveAs).add_Click((EventHandler)cmdSaveAs_Click);
			((Control)cmdSave).set_Enabled(false);
			((Control)cmdSave).set_Location(new Point(13, 128));
			((Control)cmdSave).set_Name("cmdSave");
			((Control)cmdSave).set_Size(new Size(118, 37));
			((Control)cmdSave).set_TabIndex(4);
			((Control)cmdSave).set_Text("Save\r\n[file name]");
			((ButtonBase)cmdSave).set_UseVisualStyleBackColor(true);
			((Control)cmdSave).add_Click((EventHandler)cmdSave_Click);
			((Control)cmdImport).set_Location(new Point(138, 75));
			((Control)cmdImport).set_Name("cmdImport");
			((Control)cmdImport).set_Size(new Size(89, 37));
			((Control)cmdImport).set_TabIndex(3);
			((Control)cmdImport).set_Text("&Import site file");
			((ButtonBase)cmdImport).set_UseVisualStyleBackColor(true);
			((Control)cmdImport).add_Click((EventHandler)cmdImport_Click);
			((Control)cmdOpen).set_Location(new Point(13, 75));
			((Control)cmdOpen).set_Name("cmdOpen");
			((Control)cmdOpen).set_Size(new Size(89, 37));
			((Control)cmdOpen).set_TabIndex(2);
			((Control)cmdOpen).set_Text("&Open site file");
			((ButtonBase)cmdOpen).set_UseVisualStyleBackColor(true);
			((Control)cmdOpen).add_Click((EventHandler)cmdOpen_Click);
			((Control)label14).set_AutoSize(true);
			((Control)label14).set_Location(new Point(9, 25));
			((Control)label14).set_Name("label14");
			((Control)label14).set_Size(new Size(46, 13));
			((Control)label14).set_TabIndex(0);
			((Control)label14).set_Text("Site files");
			((ListControl)cmbSiteFiles).set_FormattingEnabled(true);
			((Control)cmbSiteFiles).set_Location(new Point(12, 41));
			((Control)cmbSiteFiles).set_Name("cmbSiteFiles");
			((Control)cmbSiteFiles).set_Size(new Size(214, 21));
			((Control)cmbSiteFiles).set_TabIndex(1);
			((Control)cmdSortName).set_Location(new Point(403, 45));
			((Control)cmdSortName).set_Name("cmdSortName");
			((Control)cmdSortName).set_Size(new Size(74, 41));
			((Control)cmdSortName).set_TabIndex(5);
			((Control)cmdSortName).set_Text("Sort by Name");
			((ButtonBase)cmdSortName).set_UseVisualStyleBackColor(true);
			((Control)cmdSortName).add_Click((EventHandler)cmdSortName_Click);
			((Control)cmdSortByLongitude).set_Location(new Point(403, 102));
			((Control)cmdSortByLongitude).set_Name("cmdSortByLongitude");
			((Control)cmdSortByLongitude).set_Size(new Size(74, 41));
			((Control)cmdSortByLongitude).set_TabIndex(6);
			((Control)cmdSortByLongitude).set_Text("Sort by Longitude");
			((ButtonBase)cmdSortByLongitude).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByLongitude).add_Click((EventHandler)cmdSortByLongitude_Click);
			((Control)cmdSortByLatitude).set_Location(new Point(403, 159));
			((Control)cmdSortByLatitude).set_Name("cmdSortByLatitude");
			((Control)cmdSortByLatitude).set_Size(new Size(74, 41));
			((Control)cmdSortByLatitude).set_TabIndex(7);
			((Control)cmdSortByLatitude).set_Text("Sort by Latitude");
			((ButtonBase)cmdSortByLatitude).set_UseVisualStyleBackColor(true);
			((Control)cmdSortByLatitude).add_Click((EventHandler)cmdSortByLatitude_Click);
			((Control)label12).set_AutoSize(true);
			((Control)label12).set_Location(new Point(245, 8));
			((Control)label12).set_Name("label12");
			((Control)label12).set_Size(new Size(54, 13));
			((Control)label12).set_TabIndex(2);
			((Control)label12).set_Text("Longitude");
			((Control)label13).set_AutoSize(true);
			((Control)label13).set_Location(new Point(318, 8));
			((Control)label13).set_Name("label13");
			((Control)label13).set_Size(new Size(45, 13));
			((Control)label13).set_TabIndex(3);
			((Control)label13).set_Text("Latitude");
			((Control)panelList).set_Anchor((AnchorStyles)0);
			((Control)panelList).get_Controls().Add((Control)(object)cmdDelete);
			((Control)panelList).get_Controls().Add((Control)(object)cmdSortByLatitude);
			((Control)panelList).get_Controls().Add((Control)(object)cmdAdd);
			((Control)panelList).get_Controls().Add((Control)(object)cmdSortByLongitude);
			((Control)panelList).get_Controls().Add((Control)(object)cmdEdit);
			((Control)panelList).get_Controls().Add((Control)(object)cmdSortName);
			((Control)panelList).get_Controls().Add((Control)(object)cmdClear);
			((Control)panelList).get_Controls().Add((Control)(object)label1);
			((Control)panelList).get_Controls().Add((Control)(object)lstSiteNames);
			((Control)panelList).get_Controls().Add((Control)(object)label12);
			((Control)panelList).get_Controls().Add((Control)(object)label13);
			((Control)panelList).get_Controls().Add((Control)(object)label16);
			((Control)panelList).set_Location(new Point(8, 27));
			((Control)panelList).set_Name("panelList");
			((Control)panelList).set_Size(new Size(493, 279));
			((Control)panelList).set_TabIndex(0);
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 6.75f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(124, 11));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(83, 12));
			((Control)label16).set_TabIndex(1);
			((Control)label16).set_Text("Double-click to edit");
			((ToolStrip)menuStrip1).get_Items().AddRange((ToolStripItem[])(object)new ToolStripItem[2]
			{
				(ToolStripItem)helpToolStripMenuItem,
				(ToolStripItem)exitToolStripMenuItem
			});
			((Control)menuStrip1).set_Location(new Point(0, 0));
			((Control)menuStrip1).set_Name("menuStrip1");
			((Control)menuStrip1).set_Size(new Size(773, 24));
			((Control)menuStrip1).set_TabIndex(2);
			((Control)menuStrip1).set_Text("menuStrip1");
			((ToolStripItem)helpToolStripMenuItem).set_Image((Image)Resources.help);
			((ToolStripItem)helpToolStripMenuItem).set_Name("helpToolStripMenuItem");
			((ToolStripItem)helpToolStripMenuItem).set_Size(new Size(72, 20));
			((ToolStripItem)helpToolStripMenuItem).set_Text("&Help    ");
			((ToolStripItem)helpToolStripMenuItem).add_Click((EventHandler)helpToolStripMenuItem_Click);
			((ToolStripItem)exitToolStripMenuItem).set_Image((Image)Resources.error);
			((ToolStripItem)exitToolStripMenuItem).set_Name("exitToolStripMenuItem");
			((ToolStripItem)exitToolStripMenuItem).set_Size(new Size(53, 20));
			((ToolStripItem)exitToolStripMenuItem).set_Text("E&xit");
			((ToolStripItem)exitToolStripMenuItem).add_Click((EventHandler)exitToolStripMenuItem_Click);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(773, 695));
			((Control)this).get_Controls().Add((Control)(object)panelList);
			((Control)this).get_Controls().Add((Control)(object)pnlEdit);
			((Control)this).get_Controls().Add((Control)(object)grpEdit);
			((Control)this).get_Controls().Add((Control)(object)menuStrip1);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationDefaultsSiteEditor", true, (DataSourceUpdateMode)1));
			((Control)this).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Form)this).set_Location(Settings.Default.LocationDefaultsSiteEditor);
			((Form)this).set_MainMenuStrip(menuStrip1);
			((Control)this).set_MinimumSize(new Size(789, 731));
			((Control)this).set_Name("SiteEditor");
			((Form)this).set_StartPosition((FormStartPosition)1);
			((Control)this).set_Text("Site  Editor");
			((Form)this).add_FormClosing(new FormClosingEventHandler(SiteEditor_FormClosing));
			((Form)this).add_Load((EventHandler)SiteEditor_Load);
			((ISupportInitialize)updnAperture).EndInit();
			((ISupportInitialize)updnMagCorrn).EndInit();
			((ISupportInitialize)updnGrazeDist).EndInit();
			((Control)grpEdit).ResumeLayout(false);
			((Control)grpEdit).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)panelDDD).ResumeLayout(false);
			((Control)panelDDD).PerformLayout();
			((Control)panelDMM).ResumeLayout(false);
			((Control)panelDMM).PerformLayout();
			((Control)panelDMS).ResumeLayout(false);
			((Control)panelDMS).PerformLayout();
			((ISupportInitialize)updnTimeZone).EndInit();
			((Control)pnlEdit).ResumeLayout(false);
			((Control)grpOpen).ResumeLayout(false);
			((Control)grpOpen).PerformLayout();
			((Control)panelList).ResumeLayout(false);
			((Control)panelList).PerformLayout();
			((Control)menuStrip1).ResumeLayout(false);
			((Control)menuStrip1).PerformLayout();
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
