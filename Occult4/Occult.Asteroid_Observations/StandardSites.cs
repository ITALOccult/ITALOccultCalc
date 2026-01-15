using System;
using System.ComponentModel;
using System.Drawing;
using System.IO;
using System.Text;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class StandardSites : Form
	{
		private bool UpdatingData;

		private string SiteFile = Utilities.AppPath + "\\Sites\\ObserverSites.csv";

		private string SiteFileBUP = Utilities.AppPath + "\\Sites\\ObserverSites.bup";

		private bool SiteListChanged;

		private string FileHeader = "ID, Longitude, Latitude, Alt(m), Datum,Tel Aperture, Tel Type, ObsMethod, TimeSource,Observer1, Observer2,MoreThan2,Near,Country";

		private IContainer components;

		internal Button cmdHelpCountryCodes;

		private Label label21;

		private Label label47;

		internal TextBox txtStateCountry;

		private Label label46;

		internal TextBox txtLocatedNear;

		private Label label45;

		private Label label44;

		internal CheckBox chkEtAl;

		internal TextBox txtObserver2;

		internal TextBox txtObserver1;

		private Panel panelDDD;

		internal TextBox txtLongDDD;

		internal TextBox txtLatDDD;

		private Panel panelDMM;

		internal TextBox txtLongDmm;

		internal TextBox txtLatMM;

		internal TextBox txtLatDmm;

		internal TextBox txtLongMM;

		internal TextBox txtAlt_ft;

		private Panel panelDMS;

		internal TextBox txtLongDeg;

		internal TextBox txtLatSec;

		internal TextBox txtLatMin;

		internal TextBox txtLatDeg;

		internal TextBox txtLongSec;

		internal TextBox txtLongMin;

		private Label label20;

		private Label label19;

		private Label label18;

		private Label label17;

		private Label label16;

		private Panel panel2;

		private RadioButton optFeet;

		private RadioButton optMeters;

		private ComboBox cmbDatum;

		internal ComboBox cmbTelescope;

		internal TextBox txtAperture;

		internal TextBox txtAlt_m;

		private Panel panel1;

		internal RadioButton optDMM;

		internal RadioButton optDDD;

		internal RadioButton optDMS;

		private Label label49;

		private Button cmdAltitude;

		private Button cmdImportFromEditor;

		private Button cmdExport;

		private ListBox lstSites;

		private Button cmdAdd;

		private Label label1;

		private Panel pnlObservers;

		private Label label3;

		private Label label4;

		private TextBox txtSiteID;

		private Button cmdDelete;

		private Panel panel3;

		private Label label33;

		private ComboBox cmbMethod;

		private ComboBox cmbTime;

		private Label label32;

		private Button cmdToDetails;

		private Button cmdExit;

		private Button cmdHelp;

		private Panel panel4;

		public StandardSites()
		{
			InitializeComponent();
			((Control)txtAlt_ft).set_Top(((Control)txtAlt_m).get_Top());
			ComboBox obj = cmbDatum;
			ComboBox obj2 = cmbTelescope;
			ComboBox obj3 = cmbMethod;
			int num;
			((ListControl)cmbTime).set_SelectedIndex(num = 0);
			int num2;
			((ListControl)obj3).set_SelectedIndex(num2 = num);
			int selectedIndex;
			((ListControl)obj2).set_SelectedIndex(selectedIndex = num2);
			((ListControl)obj).set_SelectedIndex(selectedIndex);
			RadioButton obj4 = optDMS;
			bool @checked;
			optMeters.set_Checked(@checked = true);
			obj4.set_Checked(@checked);
			TextBox obj5 = txtAlt_ft;
			TextBox obj6 = txtAlt_m;
			TextBox obj7 = txtAperture;
			TextBox obj8 = txtObserver1;
			TextBox obj9 = txtObserver2;
			TextBox obj10 = txtLocatedNear;
			string text;
			((Control)txtStateCountry).set_Text(text = "");
			string text2;
			((Control)obj10).set_Text(text2 = text);
			string text3;
			((Control)obj9).set_Text(text3 = text2);
			string text4;
			((Control)obj8).set_Text(text4 = text3);
			string text5;
			((Control)obj7).set_Text(text5 = text4);
			string text6;
			((Control)obj6).set_Text(text6 = text5);
			((Control)obj5).set_Text(text6);
			chkEtAl.set_Checked(false);
			TextBox obj11 = txtLongDDD;
			TextBox obj12 = txtLongDeg;
			TextBox obj13 = txtLongDmm;
			TextBox obj14 = txtLongMin;
			TextBox obj15 = txtLongMM;
			((Control)txtLongSec).set_Text(text2 = "");
			((Control)obj15).set_Text(text3 = text2);
			((Control)obj14).set_Text(text4 = text3);
			((Control)obj13).set_Text(text5 = text4);
			((Control)obj12).set_Text(text6 = text5);
			((Control)obj11).set_Text(text6);
			TextBox obj16 = txtLatDDD;
			TextBox obj17 = txtLatDeg;
			TextBox obj18 = txtLatDmm;
			TextBox obj19 = txtLatMin;
			TextBox obj20 = txtLatMM;
			((Control)txtLatSec).set_Text(text2 = "");
			((Control)obj20).set_Text(text3 = text2);
			((Control)obj19).set_Text(text4 = text3);
			((Control)obj18).set_Text(text5 = text4);
			((Control)obj17).set_Text(text6 = text5);
			((Control)obj16).set_Text(text6);
			((Control)panelDDD).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDMM).set_Top(((Control)panelDMS).get_Top());
			((Control)panelDDD).set_Left(((Control)panelDMS).get_Left());
			((Control)panelDMM).set_Left(((Control)panelDMS).get_Left());
			AltitudeVisibility();
			DMSformatVisibility();
			ReadSites();
			ImportFromEditor();
		}

		private void txtObserver1_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtObserver1).get_Text(), "Observer 1", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtObserver1).set_Text(RevisedText.Replace(",", " "));
				((Control)txtObserver1).Focus();
			}
		}

		private void txtObserver2_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtObserver2).get_Text(), "Observer 2", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtObserver2).set_Text(RevisedText.Replace(",", " "));
				((Control)txtObserver2).Focus();
			}
		}

		private void cmdHelpCountryCodes_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Country Codes");
		}

		private void txtLocatedNear_Leave(object sender, EventArgs e)
		{
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtLocatedNear).get_Text(), "Located near", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtLocatedNear).set_Text(RevisedText.Replace(",", " "));
				((Control)txtLocatedNear).Focus();
			}
		}

		private void cmbTelescope_SelectedIndexChanged(object sender, EventArgs e)
		{
		}

		private void txtLongDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLongSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatDeg_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatMin_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
		}

		private void txtLatSec_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(0);
				UpdatingData = false;
			}
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

		private void txtStateCountry_Leave(object sender, EventArgs e)
		{
			((Control)txtStateCountry).set_Text(((Control)txtStateCountry).get_Text().ToUpper());
			if (!Utilities.CheckForPipesAndNonASCII(((Control)txtStateCountry).get_Text(), "State-Country", CheckForPipes: true, out var RevisedText))
			{
				((Control)txtStateCountry).set_Text(RevisedText.ToUpper());
				((Control)txtStateCountry).Focus();
			}
		}

		private void txtLongDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLongMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatDmm_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLatMM_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(1);
				UpdatingData = false;
			}
		}

		private void txtLongDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLongitudes(2);
				UpdatingData = false;
			}
		}

		private void txtLatDDD_Leave(object sender, EventArgs e)
		{
			if (!UpdatingData)
			{
				UpdatingData = true;
				UpdateLatitudes(2);
				UpdatingData = false;
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

		private void UpdateLongitudes(int style)
		{
			double result = 0.0;
			double result2 = 0.0;
			double result3 = 0.0;
			bool flag = false;
			switch (style)
			{
			case 0:
				flag = ((Control)txtLongDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLongSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLongDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLongMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLongDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLongDDD).get_Text().Replace('-', ' '), out result))
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
			((Control)txtLongDeg).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMin).set_Text(text.Substring(5, 2).Replace(" ", ""));
			((Control)txtLongSec).set_Text(text.Substring(8).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 4, 3, MinutesOnly: true);
			((Control)txtLongDmm).set_Text(text.Substring(0, 4).Replace(" ", ""));
			((Control)txtLongMM).set_Text(text.Substring(5).Replace(" ", ""));
			((Control)txtLongDDD).set_Text(string.Format("{0,2:F6}", num));
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
				flag = ((Control)txtLatDeg).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDeg).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMin).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				if (!double.TryParse(((Control)txtLatSec).get_Text().Replace('-', ' '), out result3))
				{
					result3 = 0.0;
				}
				break;
			case 1:
				flag = ((Control)txtLatDmm).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDmm).get_Text().Replace('-', ' '), out result))
				{
					result = 0.0;
				}
				if (!double.TryParse(((Control)txtLatMM).get_Text().Replace('-', ' '), out result2))
				{
					result2 = 0.0;
				}
				break;
			default:
				flag = ((Control)txtLatDDD).get_Text().Contains("-");
				if (!double.TryParse(((Control)txtLatDDD).get_Text().Replace('-', ' '), out result))
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
			((Control)txtLatDeg).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMin).set_Text(text.Substring(4, 2).Replace(" ", ""));
			((Control)txtLatSec).set_Text(text.Substring(7).Replace(" ", ""));
			text = Utilities.DEGtoDMS(num, 3, 3, MinutesOnly: true);
			((Control)txtLatDmm).set_Text(text.Substring(0, 3).Replace(" ", ""));
			((Control)txtLatMM).set_Text(text.Substring(4).Replace(" ", ""));
			((Control)txtLatDDD).set_Text(string.Format("{0,2:F6}", num));
		}

		private void cmdImportFromEditor_Click(object sender, EventArgs e)
		{
			ImportFromEditor();
		}

		private void ImportFromEditor()
		{
			optDMS.set_Checked(true);
			optMeters.set_Checked(true);
			Application.DoEvents();
			((Control)txtObserver1).set_Text(((Control)Data_and_Plots.Observations_Editor.txtObserver1).get_Text());
			((Control)txtObserver2).set_Text(((Control)Data_and_Plots.Observations_Editor.txtObserver2).get_Text());
			chkEtAl.set_Checked(Data_and_Plots.Observations_Editor.chkEtAl.get_Checked());
			((Control)txtLocatedNear).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLocatedNear).get_Text());
			((Control)txtStateCountry).set_Text(((Control)Data_and_Plots.Observations_Editor.txtStateCountry).get_Text());
			((Control)txtAperture).set_Text(((Control)Data_and_Plots.Observations_Editor.txtAperture).get_Text());
			((ListControl)cmbTelescope).set_SelectedIndex(((ListControl)Data_and_Plots.Observations_Editor.cmbTelescope).get_SelectedIndex());
			((Control)txtLongDeg).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLongDeg).get_Text());
			((Control)txtLongMin).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLongMin).get_Text());
			((Control)txtLongSec).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLongSec).get_Text());
			((Control)txtLatDeg).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLatDeg).get_Text());
			((Control)txtLatMin).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLatMin).get_Text());
			((Control)txtLatSec).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLatSec).get_Text());
			((Control)txtLongDmm).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLongDmm).get_Text());
			((Control)txtLongMM).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLongMM).get_Text());
			((Control)txtLatDmm).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLatDmm).get_Text());
			((Control)txtLatMM).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLatMM).get_Text());
			((Control)txtLongDDD).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLongDDD).get_Text());
			((Control)txtLatDDD).set_Text(((Control)Data_and_Plots.Observations_Editor.txtLatDDD).get_Text());
			((Control)txtAlt_ft).set_Text(((Control)Data_and_Plots.Observations_Editor.txtAlt_ft).get_Text());
			((Control)txtAlt_m).set_Text(((Control)Data_and_Plots.Observations_Editor.txtAlt_m).get_Text());
			((ListControl)cmbDatum).set_SelectedIndex(((ListControl)Data_and_Plots.Observations_Editor.cmbDatum).get_SelectedIndex());
			((ListControl)cmbMethod).set_SelectedIndex(((ListControl)Data_and_Plots.Observations_Editor.cmbMethod).get_SelectedIndex());
			((ListControl)cmbTime).set_SelectedIndex(((ListControl)Data_and_Plots.Observations_Editor.cmbTime).get_SelectedIndex());
			UpdateLongitudes(0);
			UpdateLatitudes(0);
			AltitudeVisibility();
		}

		private void cmdExport_Click(object sender, EventArgs e)
		{
			Data_and_Plots.Observations_Editor.optDMS.set_Checked(true);
			Data_and_Plots.Observations_Editor.optMeters.set_Checked(true);
			Application.DoEvents();
			((Control)Data_and_Plots.Observations_Editor.txtObserver1).set_Text(((Control)txtObserver1).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtObserver2).set_Text(((Control)txtObserver2).get_Text());
			Data_and_Plots.Observations_Editor.chkEtAl.set_Checked(chkEtAl.get_Checked());
			((Control)Data_and_Plots.Observations_Editor.txtLocatedNear).set_Text(((Control)txtLocatedNear).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtStateCountry).set_Text(((Control)txtStateCountry).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtAperture).set_Text(((Control)txtAperture).get_Text());
			((ListControl)Data_and_Plots.Observations_Editor.cmbTelescope).set_SelectedIndex(((ListControl)cmbTelescope).get_SelectedIndex());
			((Control)Data_and_Plots.Observations_Editor.txtLongDeg).set_Text(((Control)txtLongDeg).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLongMin).set_Text(((Control)txtLongMin).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLongSec).set_Text(((Control)txtLongSec).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLatDeg).set_Text(((Control)txtLatDeg).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLatMin).set_Text(((Control)txtLatMin).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLatSec).set_Text(((Control)txtLatSec).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLongDmm).set_Text(((Control)txtLongDmm).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLongMM).set_Text(((Control)txtLongMM).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLatDmm).set_Text(((Control)txtLatDmm).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLatMM).set_Text(((Control)txtLatMM).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLongDDD).set_Text(((Control)txtLongDDD).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtLatDDD).set_Text(((Control)txtLatDDD).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtAlt_ft).set_Text(((Control)txtAlt_ft).get_Text());
			((Control)Data_and_Plots.Observations_Editor.txtAlt_m).set_Text(((Control)txtAlt_m).get_Text());
			((ListControl)Data_and_Plots.Observations_Editor.cmbDatum).set_SelectedIndex(((ListControl)cmbDatum).get_SelectedIndex());
			((ListControl)Data_and_Plots.Observations_Editor.cmbMethod).set_SelectedIndex(((ListControl)cmbMethod).get_SelectedIndex());
			((ListControl)Data_and_Plots.Observations_Editor.cmbTime).set_SelectedIndex(((ListControl)cmbTime).get_SelectedIndex());
			Data_and_Plots.Observations_Editor.UpdateLongitudes(0);
			Data_and_Plots.Observations_Editor.UpdateLatitudes(0);
			Data_and_Plots.Observations_Editor.AltitudeVisibility();
		}

		private string CreateLine()
		{
			try
			{
				StringBuilder stringBuilder = new StringBuilder();
				string text = ((Control)txtSiteID).get_Text().Trim();
				if (text.Length < 1)
				{
					text = "Line " + (lstSites.get_Items().get_Count() + 1);
				}
				stringBuilder.Append(text.PadRight(15).Substring(0, 15) + ",");
				stringBuilder.Append(Formatted_Longitude() + "," + Formatted_Latitude() + "," + Formatted_Altitude() + cmbDatum.get_Items().get_Item(((ListControl)cmbDatum).get_SelectedIndex()).ToString()!.Trim() + ",");
				stringBuilder.Append(Formatted_Telescope() + cmbMethod.get_Items().get_Item(((ListControl)cmbMethod).get_SelectedIndex()).ToString()!.Trim() + "," + cmbTime.get_Items().get_Item(((ListControl)cmbTime).get_SelectedIndex()).ToString()!.Trim() + ",");
				stringBuilder.Append(FormattedObservers());
				return stringBuilder.ToString();
			}
			catch
			{
				return "";
			}
		}

		private string Formatted_Longitude()
		{
			try
			{
				int.TryParse(((Control)txtLongDeg).get_Text().Replace("-", ""), out var result);
				int.TryParse(((Control)txtLongMin).get_Text(), out var result2);
				double.TryParse(((Control)txtLongSec).get_Text(), out var result3);
				if (((Control)txtLongDeg).get_Text().Contains("-"))
				{
					return string.Format("-{0,3:F0}{1,3:F0}", result, result2) + Utilities.FormatNumber(result3, 3, 1).PadRight(5);
				}
				return string.Format("+{0,3:F0}{1,3:F0}", result, result2) + Utilities.FormatNumber(result3, 3, 1).PadRight(5);
			}
			catch
			{
				return "";
			}
		}

		private string Formatted_Latitude()
		{
			try
			{
				int.TryParse(((Control)txtLatDeg).get_Text().Replace("-", ""), out var result);
				int.TryParse(((Control)txtLatMin).get_Text(), out var result2);
				double.TryParse(((Control)txtLatSec).get_Text(), out var result3);
				if (((Control)txtLatDeg).get_Text().Contains("-"))
				{
					return string.Format("-{0,2:F0}{1,3:F0}", result, result2) + Utilities.FormatNumber(result3, 3, 1).PadRight(5);
				}
				return string.Format("+{0,2:F0}{1,3:F0}", result, result2) + Utilities.FormatNumber(result3, 3, 1).PadRight(5);
			}
			catch
			{
				return "";
			}
		}

		private string Formatted_Altitude()
		{
			double.TryParse(((Control)txtAlt_m).get_Text(), out var result);
			return string.Format("{0,5},", result);
		}

		private string Formatted_Telescope()
		{
			try
			{
				double.TryParse(((Control)txtAperture).get_Text(), out var result);
				return string.Format("{0,4},", result) + cmbTelescope.get_Items().get_Item(((ListControl)cmbTelescope).get_SelectedIndex()).ToString() + ",";
			}
			catch
			{
				return "";
			}
		}

		private string FormattedObservers()
		{
			StringBuilder stringBuilder = new StringBuilder();
			stringBuilder.Append(((Control)txtObserver1).get_Text().Trim().Replace(",", ".") + "," + ((Control)txtObserver2).get_Text().Replace(",", ".") + ",");
			if (chkEtAl.get_Checked())
			{
				stringBuilder.Append("+");
			}
			stringBuilder.Append(",");
			stringBuilder.Append(((Control)txtLocatedNear).get_Text().Replace(",", ".") + ",");
			stringBuilder.Append(((Control)txtStateCountry).get_Text().Replace(",", "."));
			return stringBuilder.ToString();
		}

		private void cmdAdd_Click(object sender, EventArgs e)
		{
			//IL_0031: Unknown result type (might be due to invalid IL or missing references)
			string text = CreateLine();
			if (text.Split(new char[1] { ',' }).Length < 14)
			{
				MessageBox.Show("Not all the Observer fields have been (correctly) filled,\r\nand will not be added to the list.\r\n\r\nAll fields must be correctly completed.\r\n\r\nReview and try again.", "Incomplete data error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			lstSites.get_Items().Add((object)text);
			SiteListChanged = true;
		}

		private void cmdDelete_Click(object sender, EventArgs e)
		{
			//IL_0017: Unknown result type (might be due to invalid IL or missing references)
			//IL_001d: Invalid comparison between Unknown and I4
			if ((int)MessageBox.Show("Are you sure you want to delete the selected site?", "Confirm delete", (MessageBoxButtons)4, (MessageBoxIcon)32, (MessageBoxDefaultButton)256, (MessageBoxOptions)2097152) != 7)
			{
				lstSites.get_Items().RemoveAt(((ListControl)lstSites).get_SelectedIndex());
				SiteListChanged = true;
			}
		}

		private void StandardSites_FormClosing(object sender, FormClosingEventArgs e)
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			//IL_0024: Unknown result type (might be due to invalid IL or missing references)
			//IL_0025: Unknown result type (might be due to invalid IL or missing references)
			//IL_0027: Invalid comparison between Unknown and I4
			//IL_0031: Unknown result type (might be due to invalid IL or missing references)
			//IL_0033: Invalid comparison between Unknown and I4
			if (SiteListChanged)
			{
				DialogResult val = MessageBox.Show("Do you want to Save the current list?", "Confirm Save/Cancel", (MessageBoxButtons)3, (MessageBoxIcon)32, (MessageBoxDefaultButton)512, (MessageBoxOptions)2097152);
				if ((int)val == 2)
				{
					((CancelEventArgs)(object)e).Cancel = true;
				}
				else if ((int)val == 6)
				{
					WriteSites();
				}
			}
		}

		private void ReadSites()
		{
			if (!File.Exists(SiteFile))
			{
				return;
			}
			using StreamReader streamReader = new StreamReader(SiteFile);
			do
			{
				string text = streamReader.ReadLine();
				if (!(text.Substring(0, 3) == "ID,") && text.Length > 50)
				{
					lstSites.get_Items().Add((object)text);
				}
			}
			while (!streamReader.EndOfStream);
		}

		private void WriteSites()
		{
			//IL_001f: Unknown result type (might be due to invalid IL or missing references)
			if (lstSites.get_Items().get_Count() == 0)
			{
				MessageBox.Show("No sites are listed. Nothing to save", "No sites", (MessageBoxButtons)0, (MessageBoxIcon)16);
				return;
			}
			if (File.Exists(SiteFileBUP))
			{
				File.Delete(SiteFileBUP);
			}
			if (File.Exists(SiteFile))
			{
				File.Move(SiteFile, SiteFileBUP);
			}
			using StreamWriter streamWriter = new StreamWriter(SiteFile);
			streamWriter.WriteLine(FileHeader);
			for (int i = 0; i < lstSites.get_Items().get_Count(); i++)
			{
				streamWriter.WriteLine(lstSites.get_Items().get_Item(i).ToString());
			}
		}

		private void cmdToDetails_Click(object sender, EventArgs e)
		{
			//IL_005a: Unknown result type (might be due to invalid IL or missing references)
			if (((ListControl)lstSites).get_SelectedIndex() < 0)
			{
				return;
			}
			string[] array = lstSites.get_Items().get_Item(((ListControl)lstSites).get_SelectedIndex()).ToString()!.Split(new char[1] { ',' });
			if (array.Length < 14)
			{
				MessageBox.Show("This line is incorrectly formatted and cannot be interpreted.", "File error", (MessageBoxButtons)0, (MessageBoxIcon)16, (MessageBoxDefaultButton)0, (MessageBoxOptions)2097152);
				return;
			}
			((Control)txtSiteID).set_Text(array[0]);
			RadioButton obj = optDMS;
			bool @checked;
			optMeters.set_Checked(@checked = true);
			obj.set_Checked(@checked);
			((Control)txtLongDeg).set_Text(array[1].Substring(0, 4).Replace("+", "").Replace(" ", "")
				.Trim());
			((Control)txtLongMin).set_Text(array[1].Substring(5, 2).Trim());
			((Control)txtLongSec).set_Text(array[1].Substring(8, 4).Trim());
			((Control)txtLatDeg).set_Text(array[2].Substring(0, 3).Replace("+", "").Replace(" ", "")
				.Trim());
			((Control)txtLatMin).set_Text(array[2].Substring(4, 2).Trim());
			((Control)txtLatSec).set_Text(array[2].Substring(7, 4).Trim());
			((Control)txtAlt_m).set_Text(array[3].Trim());
			double.TryParse(array[3], out var result);
			((Control)txtAlt_ft).set_Text(string.Format("{0,1:f0}", result / 0.3048));
			UpdateLongitudes(0);
			UpdateLatitudes(0);
			AltitudeVisibility();
			for (int i = 0; i < cmbDatum.get_Items().get_Count(); i++)
			{
				if (array[4].Trim() == cmbDatum.get_Items().get_Item(i).ToString()!.Trim())
				{
					((ListControl)cmbDatum).set_SelectedIndex(i);
				}
			}
			((Control)txtAperture).set_Text(array[5].Trim());
			for (int j = 0; j < cmbTelescope.get_Items().get_Count(); j++)
			{
				if (array[6].Trim() == cmbTelescope.get_Items().get_Item(j).ToString()!.Trim())
				{
					((ListControl)cmbTelescope).set_SelectedIndex(j);
				}
			}
			for (int k = 0; k < cmbMethod.get_Items().get_Count(); k++)
			{
				if (array[7].Trim() == cmbMethod.get_Items().get_Item(k).ToString()!.Trim())
				{
					((ListControl)cmbMethod).set_SelectedIndex(k);
				}
			}
			for (int l = 0; l < cmbTime.get_Items().get_Count(); l++)
			{
				if (array[8].Trim() == cmbTime.get_Items().get_Item(l).ToString()!.Trim())
				{
					((ListControl)cmbTime).set_SelectedIndex(l);
				}
			}
			((Control)txtObserver1).set_Text(array[9].Trim());
			((Control)txtObserver2).set_Text(array[10].Trim());
			chkEtAl.set_Checked(array[11].Trim().Length > 0);
			((Control)txtLocatedNear).set_Text(array[12].Trim());
			((Control)txtStateCountry).set_Text(array[13].Trim());
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Asteroid-AutoFillObserver");
		}

		private void cmdExit_Click(object sender, EventArgs e)
		{
			((Form)this).Close();
			((Component)this).Dispose();
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
			//IL_2ad5: Unknown result type (might be due to invalid IL or missing references)
			//IL_2adf: Expected O, but got Unknown
			label21 = new Label();
			label47 = new Label();
			txtStateCountry = new TextBox();
			label46 = new Label();
			txtLocatedNear = new TextBox();
			label45 = new Label();
			label44 = new Label();
			chkEtAl = new CheckBox();
			txtObserver2 = new TextBox();
			txtObserver1 = new TextBox();
			panelDDD = new Panel();
			txtLongDDD = new TextBox();
			txtLatDDD = new TextBox();
			panelDMM = new Panel();
			txtLongDmm = new TextBox();
			txtLatMM = new TextBox();
			txtLatDmm = new TextBox();
			txtLongMM = new TextBox();
			txtAlt_ft = new TextBox();
			panelDMS = new Panel();
			txtLongDeg = new TextBox();
			txtLatSec = new TextBox();
			txtLatMin = new TextBox();
			txtLatDeg = new TextBox();
			txtLongSec = new TextBox();
			txtLongMin = new TextBox();
			label20 = new Label();
			label19 = new Label();
			label18 = new Label();
			label17 = new Label();
			label16 = new Label();
			panel2 = new Panel();
			optFeet = new RadioButton();
			optMeters = new RadioButton();
			cmbDatum = new ComboBox();
			cmbTelescope = new ComboBox();
			txtAperture = new TextBox();
			txtAlt_m = new TextBox();
			panel1 = new Panel();
			optDMM = new RadioButton();
			optDDD = new RadioButton();
			optDMS = new RadioButton();
			label49 = new Label();
			cmdAltitude = new Button();
			cmdImportFromEditor = new Button();
			cmdExport = new Button();
			lstSites = new ListBox();
			cmdAdd = new Button();
			label1 = new Label();
			cmdHelpCountryCodes = new Button();
			pnlObservers = new Panel();
			label3 = new Label();
			txtSiteID = new TextBox();
			label4 = new Label();
			cmdDelete = new Button();
			panel3 = new Panel();
			label33 = new Label();
			cmbMethod = new ComboBox();
			cmbTime = new ComboBox();
			label32 = new Label();
			cmdToDetails = new Button();
			cmdExit = new Button();
			cmdHelp = new Button();
			panel4 = new Panel();
			((Control)panelDDD).SuspendLayout();
			((Control)panelDMM).SuspendLayout();
			((Control)panelDMS).SuspendLayout();
			((Control)panel2).SuspendLayout();
			((Control)panel1).SuspendLayout();
			((Control)pnlObservers).SuspendLayout();
			((Control)panel3).SuspendLayout();
			((Control)panel4).SuspendLayout();
			((Control)this).SuspendLayout();
			((Control)label21).set_AutoSize(true);
			((Control)label21).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label21).set_Location(new Point(196, 73));
			((Control)label21).set_Name("label21");
			((Control)label21).set_Size(new Size(86, 13));
			((Control)label21).set_TabIndex(28);
			((Control)label21).set_Text("2 or 3 letter code");
			((Control)label47).set_AutoSize(true);
			((Control)label47).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label47).set_Location(new Point(211, 84));
			((Control)label47).set_Name("label47");
			((Control)label47).set_Size(new Size(50, 26));
			((Control)label47).set_TabIndex(7);
			((Control)label47).set_Text("country\r\nor state");
			((Control)txtStateCountry).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtStateCountry).set_Location(new Point(266, 88));
			((Control)txtStateCountry).set_Name("txtStateCountry");
			((Control)txtStateCountry).set_Size(new Size(30, 20));
			((Control)txtStateCountry).set_TabIndex(8);
			((Control)txtStateCountry).add_Leave((EventHandler)txtStateCountry_Leave);
			((Control)label46).set_AutoSize(true);
			((Control)label46).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label46).set_Location(new Point(13, 85));
			((Control)label46).set_Name("label46");
			((Control)label46).set_Size(new Size(49, 26));
			((Control)label46).set_TabIndex(5);
			((Control)label46).set_Text("located\r\nnear");
			((Control)txtLocatedNear).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLocatedNear).set_Location(new Point(63, 88));
			((Control)txtLocatedNear).set_Name("txtLocatedNear");
			((Control)txtLocatedNear).set_Size(new Size(130, 20));
			((Control)txtLocatedNear).set_TabIndex(6);
			((Control)txtLocatedNear).add_Leave((EventHandler)txtLocatedNear_Leave);
			((Control)label45).set_AutoSize(true);
			((Control)label45).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label45).set_Location(new Point(166, 46));
			((Control)label45).set_Name("label45");
			((Control)label45).set_Size(new Size(50, 13));
			((Control)label45).set_TabIndex(2);
			((Control)label45).set_Text("Name 2");
			((Control)label44).set_AutoSize(true);
			((Control)label44).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label44).set_Location(new Point(12, 46));
			((Control)label44).set_Name("label44");
			((Control)label44).set_Size(new Size(50, 13));
			((Control)label44).set_TabIndex(0);
			((Control)label44).set_Text("Name 1");
			((Control)chkEtAl).set_AutoSize(true);
			((Control)chkEtAl).set_BackColor(Color.Honeydew);
			chkEtAl.set_CheckAlign(ContentAlignment.MiddleRight);
			((Control)chkEtAl).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)chkEtAl).set_Location(new Point(11, 66));
			((Control)chkEtAl).set_Name("chkEtAl");
			((Control)chkEtAl).set_Size(new Size(153, 17));
			((Control)chkEtAl).set_TabIndex(4);
			((Control)chkEtAl).set_Text("More than 2 observers");
			((ButtonBase)chkEtAl).set_UseVisualStyleBackColor(false);
			((Control)txtObserver2).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserver2).set_Location(new Point(216, 43));
			((Control)txtObserver2).set_Name("txtObserver2");
			((Control)txtObserver2).set_Size(new Size(85, 20));
			((Control)txtObserver2).set_TabIndex(3);
			((Control)txtObserver2).add_Leave((EventHandler)txtObserver2_Leave);
			((Control)txtObserver1).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtObserver1).set_Location(new Point(63, 43));
			((Control)txtObserver1).set_Name("txtObserver1");
			((Control)txtObserver1).set_Size(new Size(99, 20));
			((Control)txtObserver1).set_TabIndex(1);
			((Control)txtObserver1).add_Leave((EventHandler)txtObserver1_Leave);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLongDDD);
			((Control)panelDDD).get_Controls().Add((Control)(object)txtLatDDD);
			((Control)panelDDD).set_Location(new Point(294, 151));
			((Control)panelDDD).set_Name("panelDDD");
			((Control)panelDDD).set_Size(new Size(113, 52));
			((Control)panelDDD).set_TabIndex(10);
			((Control)txtLongDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDDD).set_Location(new Point(4, 2));
			((Control)txtLongDDD).set_Name("txtLongDDD");
			((Control)txtLongDDD).set_Size(new Size(82, 20));
			((Control)txtLongDDD).set_TabIndex(0);
			((Control)txtLongDDD).add_Leave((EventHandler)txtLongDDD_Leave);
			((Control)txtLatDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDDD).set_Location(new Point(4, 28));
			((Control)txtLatDDD).set_Name("txtLatDDD");
			((Control)txtLatDDD).set_Size(new Size(82, 20));
			((Control)txtLatDDD).set_TabIndex(1);
			((Control)txtLatDDD).add_Leave((EventHandler)txtLatDDD_Leave);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatMM);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLatDmm);
			((Control)panelDMM).get_Controls().Add((Control)(object)txtLongMM);
			((Control)panelDMM).set_Location(new Point(259, 96));
			((Control)panelDMM).set_Name("panelDMM");
			((Control)panelDMM).set_Size(new Size(113, 52));
			((Control)panelDMM).set_TabIndex(9);
			((Control)txtLongDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDmm).set_Location(new Point(4, 2));
			((Control)txtLongDmm).set_Name("txtLongDmm");
			((Control)txtLongDmm).set_Size(new Size(34, 20));
			((Control)txtLongDmm).set_TabIndex(0);
			((Control)txtLongDmm).add_Leave((EventHandler)txtLongDmm_Leave);
			((Control)txtLatMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMM).set_Location(new Point(44, 28));
			((Control)txtLatMM).set_Name("txtLatMM");
			((Control)txtLatMM).set_Size(new Size(44, 20));
			((Control)txtLatMM).set_TabIndex(3);
			((Control)txtLatMM).add_Leave((EventHandler)txtLatMM_Leave);
			((Control)txtLatDmm).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDmm).set_Location(new Point(4, 28));
			((Control)txtLatDmm).set_Name("txtLatDmm");
			((Control)txtLatDmm).set_Size(new Size(34, 20));
			((Control)txtLatDmm).set_TabIndex(2);
			((Control)txtLatDmm).add_Leave((EventHandler)txtLatDmm_Leave);
			((Control)txtLongMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMM).set_Location(new Point(44, 2));
			((Control)txtLongMM).set_Name("txtLongMM");
			((Control)txtLongMM).set_Size(new Size(44, 20));
			((Control)txtLongMM).set_TabIndex(1);
			((Control)txtLongMM).add_Leave((EventHandler)txtLongMM_Leave);
			((Control)txtAlt_ft).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_ft).set_Location(new Point(206, 206));
			((Control)txtAlt_ft).set_Name("txtAlt_ft");
			((Control)txtAlt_ft).set_Size(new Size(53, 20));
			((Control)txtAlt_ft).set_TabIndex(21);
			((Control)txtAlt_ft).set_Visible(false);
			((Control)txtAlt_ft).add_Leave((EventHandler)txtAlt_ft_Leave);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatMin);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLatDeg);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongSec);
			((Control)panelDMS).get_Controls().Add((Control)(object)txtLongMin);
			((Control)panelDMS).set_Location(new Point(94, 143));
			((Control)panelDMS).set_Name("panelDMS");
			((Control)panelDMS).set_Size(new Size(113, 52));
			((Control)panelDMS).set_TabIndex(14);
			((Control)txtLongDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongDeg).set_Location(new Point(4, 2));
			((Control)txtLongDeg).set_Name("txtLongDeg");
			((Control)txtLongDeg).set_Size(new Size(34, 20));
			((Control)txtLongDeg).set_TabIndex(0);
			((Control)txtLongDeg).add_Leave((EventHandler)txtLongDeg_Leave);
			((Control)txtLatSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatSec).set_Location(new Point(71, 28));
			((Control)txtLatSec).set_Name("txtLatSec");
			((Control)txtLatSec).set_Size(new Size(39, 20));
			((Control)txtLatSec).set_TabIndex(5);
			((Control)txtLatSec).add_Leave((EventHandler)txtLatSec_Leave);
			((Control)txtLatMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatMin).set_Location(new Point(44, 28));
			((Control)txtLatMin).set_Name("txtLatMin");
			((Control)txtLatMin).set_Size(new Size(21, 20));
			((Control)txtLatMin).set_TabIndex(4);
			((Control)txtLatMin).add_Leave((EventHandler)txtLatMin_Leave);
			((Control)txtLatDeg).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLatDeg).set_Location(new Point(4, 28));
			((Control)txtLatDeg).set_Name("txtLatDeg");
			((Control)txtLatDeg).set_Size(new Size(34, 20));
			((Control)txtLatDeg).set_TabIndex(3);
			((Control)txtLatDeg).add_Leave((EventHandler)txtLatDeg_Leave);
			((Control)txtLongSec).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongSec).set_Location(new Point(71, 2));
			((Control)txtLongSec).set_Name("txtLongSec");
			((Control)txtLongSec).set_Size(new Size(40, 20));
			((Control)txtLongSec).set_TabIndex(2);
			((Control)txtLongSec).add_Leave((EventHandler)txtLongSec_Leave);
			((Control)txtLongMin).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtLongMin).set_Location(new Point(44, 2));
			((Control)txtLongMin).set_Name("txtLongMin");
			((Control)txtLongMin).set_Size(new Size(21, 20));
			((Control)txtLongMin).set_TabIndex(1);
			((Control)txtLongMin).add_Leave((EventHandler)txtLongMin_Leave);
			((Control)label20).set_AutoSize(true);
			((Control)label20).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label20).set_Location(new Point(140, 122));
			((Control)label20).set_Name("label20");
			((Control)label20).set_Size(new Size(23, 13));
			((Control)label20).set_TabIndex(11);
			((Control)label20).set_Text("cm");
			((Control)label19).set_AutoSize(true);
			((Control)label19).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label19).set_Location(new Point(20, 203));
			((Control)label19).set_Name("label19");
			((Control)label19).set_Size(new Size(43, 13));
			((Control)label19).set_TabIndex(17);
			((Control)label19).set_Text("Datum");
			((Control)label18).set_AutoSize(true);
			((Control)label18).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label18).set_Location(new Point(42, 170));
			((Control)label18).set_Name("label18");
			((Control)label18).set_Size(new Size(53, 13));
			((Control)label18).set_TabIndex(15);
			((Control)label18).set_Text("Latitude");
			((Control)label17).set_AutoSize(true);
			((Control)label17).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label17).set_Location(new Point(16, 148));
			((Control)label17).set_Name("label17");
			((Control)label17).set_Size(new Size(79, 13));
			((Control)label17).set_TabIndex(13);
			((Control)label17).set_Text("E. Longitude");
			((Control)label16).set_AutoSize(true);
			((Control)label16).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label16).set_Location(new Point(42, 121));
			((Control)label16).set_Name("label16");
			((Control)label16).set_Size(new Size(55, 13));
			((Control)label16).set_TabIndex(9);
			((Control)label16).set_Text("Aperture");
			((Control)panel2).get_Controls().Add((Control)(object)optFeet);
			((Control)panel2).get_Controls().Add((Control)(object)optMeters);
			((Control)panel2).set_Location(new Point(263, 193));
			((Control)panel2).set_Name("panel2");
			((Control)panel2).set_Size(new Size(36, 34));
			((Control)panel2).set_TabIndex(22);
			((Control)optFeet).set_AutoSize(true);
			((Control)optFeet).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optFeet).set_Location(new Point(2, 16));
			((Control)optFeet).set_Name("optFeet");
			((Control)optFeet).set_Size(new Size(33, 17));
			((Control)optFeet).set_TabIndex(1);
			optFeet.set_TabStop(true);
			((Control)optFeet).set_Text("ft");
			((ButtonBase)optFeet).set_UseVisualStyleBackColor(true);
			optFeet.add_CheckedChanged((EventHandler)optFeet_CheckedChanged);
			((Control)optMeters).set_AutoSize(true);
			optMeters.set_Checked(true);
			((Control)optMeters).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optMeters).set_Location(new Point(2, 1));
			((Control)optMeters).set_Name("optMeters");
			((Control)optMeters).set_Size(new Size(34, 17));
			((Control)optMeters).set_TabIndex(0);
			optMeters.set_TabStop(true);
			((Control)optMeters).set_Text("m");
			((ButtonBase)optMeters).set_UseVisualStyleBackColor(true);
			optMeters.add_CheckedChanged((EventHandler)optMeters_CheckedChanged);
			cmbDatum.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbDatum).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbDatum).set_FormattingEnabled(true);
			cmbDatum.get_Items().AddRange(new object[6] { "WGS84", "NAD1927", "ED1950", "Tokyo", "GB1936", "?" });
			((Control)cmbDatum).set_Location(new Point(66, 200));
			((Control)cmbDatum).set_Name("cmbDatum");
			((Control)cmbDatum).set_Size(new Size(64, 21));
			((Control)cmbDatum).set_TabIndex(18);
			cmbTelescope.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTelescope).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTelescope).set_FormattingEnabled(true);
			cmbTelescope.get_Items().AddRange(new object[9] { "?", "Refractor", "Newtonian", "SCT", "Dobsonian", "Binoculars", "Other", "None", "Electronic" });
			((Control)cmbTelescope).set_Location(new Point(171, 118));
			((Control)cmbTelescope).set_Name("cmbTelescope");
			((Control)cmbTelescope).set_Size(new Size(75, 21));
			((Control)cmbTelescope).set_TabIndex(12);
			cmbTelescope.add_SelectedIndexChanged((EventHandler)cmbTelescope_SelectedIndexChanged);
			((Control)txtAperture).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAperture).set_Location(new Point(99, 118));
			((Control)txtAperture).set_Name("txtAperture");
			((Control)txtAperture).set_Size(new Size(38, 20));
			((Control)txtAperture).set_TabIndex(10);
			((Control)txtAlt_m).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtAlt_m).set_Location(new Point(206, 199));
			((Control)txtAlt_m).set_Name("txtAlt_m");
			((Control)txtAlt_m).set_Size(new Size(53, 20));
			((Control)txtAlt_m).set_TabIndex(20);
			((Control)txtAlt_m).add_Leave((EventHandler)txtAlt_m_Leave);
			((Control)panel1).get_Controls().Add((Control)(object)optDMM);
			((Control)panel1).get_Controls().Add((Control)(object)optDDD);
			((Control)panel1).get_Controls().Add((Control)(object)optDMS);
			((Control)panel1).set_Location(new Point(230, 140));
			((Control)panel1).set_Name("panel1");
			((Control)panel1).set_Size(new Size(61, 56));
			((Control)panel1).set_TabIndex(16);
			((Control)optDMM).set_AutoSize(true);
			((Control)optDMM).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDMM).set_Location(new Point(3, 20));
			((Control)optDMM).set_Name("optDMM");
			((Control)optDMM).set_Size(new Size(54, 17));
			((Control)optDMM).set_TabIndex(1);
			optDMM.set_TabStop(true);
			((Control)optDMM).set_Text("dm.m");
			((ButtonBase)optDMM).set_UseVisualStyleBackColor(true);
			optDMM.add_CheckedChanged((EventHandler)optDMM_CheckedChanged);
			((Control)optDDD).set_AutoSize(true);
			((Control)optDDD).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDDD).set_Location(new Point(3, 35));
			((Control)optDDD).set_Name("optDDD");
			((Control)optDDD).set_Size(new Size(50, 17));
			((Control)optDDD).set_TabIndex(2);
			optDDD.set_TabStop(true);
			((Control)optDDD).set_Text("d.dd");
			((ButtonBase)optDDD).set_UseVisualStyleBackColor(true);
			optDDD.add_CheckedChanged((EventHandler)optDDD_CheckedChanged);
			((Control)optDMS).set_AutoSize(true);
			optDMS.set_Checked(true);
			((Control)optDMS).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)optDMS).set_Location(new Point(3, 5));
			((Control)optDMS).set_Name("optDMS");
			((Control)optDMS).set_Size(new Size(47, 17));
			((Control)optDMS).set_TabIndex(0);
			optDMS.set_TabStop(true);
			((Control)optDMS).set_Text("dms");
			((ButtonBase)optDMS).set_UseVisualStyleBackColor(true);
			optDMS.add_CheckedChanged((EventHandler)optDMS_CheckedChanged);
			((Control)label49).set_AutoSize(true);
			((Control)label49).set_Font(new Font("Microsoft Sans Serif", 7f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)label49).set_Location(new Point(167, 32));
			((Control)label49).set_Name("label49");
			((Control)label49).set_Size(new Size(46, 13));
			((Control)label49).set_TabIndex(29);
			((Control)label49).set_Text("Optional");
			((Control)cmdAltitude).set_BackColor(Color.Honeydew);
			((ButtonBase)cmdAltitude).get_FlatAppearance().set_BorderColor(Color.LimeGreen);
			((ButtonBase)cmdAltitude).set_FlatStyle((FlatStyle)0);
			((Control)cmdAltitude).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAltitude).set_Location(new Point(152, 199));
			((Control)cmdAltitude).set_Name("cmdAltitude");
			((Control)cmdAltitude).set_Size(new Size(60, 21));
			((Control)cmdAltitude).set_TabIndex(19);
			((Control)cmdAltitude).set_Text("Altitude");
			((ButtonBase)cmdAltitude).set_UseVisualStyleBackColor(false);
			((Control)cmdImportFromEditor).set_BackColor(Color.Aquamarine);
			((Control)cmdImportFromEditor).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdImportFromEditor).set_Location(new Point(78, 326));
			((Control)cmdImportFromEditor).set_Name("cmdImportFromEditor");
			((Control)cmdImportFromEditor).set_Size(new Size(65, 35));
			((Control)cmdImportFromEditor).set_TabIndex(6);
			((Control)cmdImportFromEditor).set_Text("Import");
			((ButtonBase)cmdImportFromEditor).set_UseVisualStyleBackColor(false);
			((Control)cmdImportFromEditor).add_Click((EventHandler)cmdImportFromEditor_Click);
			((Control)cmdExport).set_BackColor(Color.LightBlue);
			((Control)cmdExport).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdExport).set_Location(new Point(199, 326));
			((Control)cmdExport).set_Name("cmdExport");
			((Control)cmdExport).set_Size(new Size(65, 35));
			((Control)cmdExport).set_TabIndex(7);
			((Control)cmdExport).set_Text("Export");
			((ButtonBase)cmdExport).set_UseVisualStyleBackColor(false);
			((Control)cmdExport).add_Click((EventHandler)cmdExport_Click);
			((Control)lstSites).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)lstSites).set_FormattingEnabled(true);
			lstSites.set_HorizontalScrollbar(true);
			((Control)lstSites).set_Location(new Point(9, 20));
			((Control)lstSites).set_Name("lstSites");
			((Control)lstSites).set_Size(new Size(520, 277));
			((Control)lstSites).set_TabIndex(8);
			((Control)cmdAdd).set_BackColor(Color.GreenYellow);
			((Control)cmdAdd).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdAdd).set_Location(new Point(33, 312));
			((Control)cmdAdd).set_Name("cmdAdd");
			((Control)cmdAdd).set_Size(new Size(140, 37));
			((Control)cmdAdd).set_TabIndex(9);
			((Control)cmdAdd).set_Text("Add to List");
			((ButtonBase)cmdAdd).set_UseVisualStyleBackColor(false);
			((Control)cmdAdd).add_Click((EventHandler)cmdAdd_Click);
			((Control)label1).set_AutoSize(true);
			((Control)label1).set_Font(new Font("Microsoft Sans Serif", 15f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label1).set_Location(new Point(589, 8));
			((Control)label1).set_Name("label1");
			((Control)label1).set_Size(new Size(121, 25));
			((Control)label1).set_TabIndex(10);
			((Control)label1).set_Text("List of sites");
			((ButtonBase)cmdHelpCountryCodes).set_Image((Image)Resources.Help16x16);
			((Control)cmdHelpCountryCodes).set_Location(new Point(283, 69));
			((Control)cmdHelpCountryCodes).set_Name("cmdHelpCountryCodes");
			((Control)cmdHelpCountryCodes).set_Size(new Size(18, 19));
			((Control)cmdHelpCountryCodes).set_TabIndex(30);
			((ButtonBase)cmdHelpCountryCodes).set_UseVisualStyleBackColor(true);
			((Control)cmdHelpCountryCodes).add_Click((EventHandler)cmdHelpCountryCodes_Click);
			((Control)pnlObservers).set_BackColor(Color.Honeydew);
			pnlObservers.set_BorderStyle((BorderStyle)2);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label33);
			((Control)pnlObservers).get_Controls().Add((Control)(object)cmbMethod);
			((Control)pnlObservers).get_Controls().Add((Control)(object)cmbTime);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label32);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label4);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtSiteID);
			((Control)pnlObservers).get_Controls().Add((Control)(object)panelDDD);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtLocatedNear);
			((Control)pnlObservers).get_Controls().Add((Control)(object)cmdHelpCountryCodes);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label21);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label49);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label47);
			((Control)pnlObservers).get_Controls().Add((Control)(object)panel1);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtStateCountry);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtAlt_m);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label46);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtAperture);
			((Control)pnlObservers).get_Controls().Add((Control)(object)cmbTelescope);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label45);
			((Control)pnlObservers).get_Controls().Add((Control)(object)cmbDatum);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label44);
			((Control)pnlObservers).get_Controls().Add((Control)(object)panel2);
			((Control)pnlObservers).get_Controls().Add((Control)(object)chkEtAl);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label16);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtObserver2);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label17);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtObserver1);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label18);
			((Control)pnlObservers).get_Controls().Add((Control)(object)panelDMM);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label19);
			((Control)pnlObservers).get_Controls().Add((Control)(object)txtAlt_ft);
			((Control)pnlObservers).get_Controls().Add((Control)(object)label20);
			((Control)pnlObservers).get_Controls().Add((Control)(object)panelDMS);
			((Control)pnlObservers).get_Controls().Add((Control)(object)cmdAltitude);
			((Control)pnlObservers).set_Location(new Point(11, 19));
			((Control)pnlObservers).set_Name("pnlObservers");
			((Control)pnlObservers).set_Size(new Size(320, 296));
			((Control)pnlObservers).set_TabIndex(12);
			((Control)label3).set_AutoSize(true);
			((Control)label3).set_Font(new Font("Microsoft Sans Serif", 15f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label3).set_Location(new Point(78, 8));
			((Control)label3).set_Name("label3");
			((Control)label3).set_Size(new Size(210, 25));
			((Control)label3).set_TabIndex(13);
			((Control)label3).set_Text("Observer site details");
			((Control)txtSiteID).set_Location(new Point(179, 7));
			((Control)txtSiteID).set_Name("txtSiteID");
			((Control)txtSiteID).set_Size(new Size(105, 20));
			((Control)txtSiteID).set_TabIndex(31);
			((Control)label4).set_AutoSize(true);
			((Control)label4).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label4).set_Location(new Point(32, 10));
			((Control)label4).set_Name("label4");
			((Control)label4).set_Size(new Size(147, 13));
			((Control)label4).set_TabIndex(32);
			((Control)label4).set_Text("Site label (15 chars max)");
			((Control)cmdDelete).set_BackColor(Color.LightCoral);
			((Control)cmdDelete).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdDelete).set_Location(new Point(361, 312));
			((Control)cmdDelete).set_Name("cmdDelete");
			((Control)cmdDelete).set_Size(new Size(140, 37));
			((Control)cmdDelete).set_TabIndex(14);
			((Control)cmdDelete).set_Text("Delete selected");
			((ButtonBase)cmdDelete).set_UseVisualStyleBackColor(false);
			((Control)cmdDelete).add_Click((EventHandler)cmdDelete_Click);
			((Control)panel3).set_BackColor(Color.BlanchedAlmond);
			panel3.set_BorderStyle((BorderStyle)2);
			((Control)panel3).get_Controls().Add((Control)(object)pnlObservers);
			((Control)panel3).get_Controls().Add((Control)(object)cmdExport);
			((Control)panel3).get_Controls().Add((Control)(object)cmdImportFromEditor);
			((Control)panel3).set_Location(new Point(10, 33));
			((Control)panel3).set_Name("panel3");
			((Control)panel3).set_Size(new Size(347, 370));
			((Control)panel3).set_TabIndex(15);
			((Control)label33).set_AutoSize(true);
			((Control)label33).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label33).set_Location(new Point(47, 266));
			((Control)label33).set_Name("label33");
			((Control)label33).set_Size(new Size(34, 13));
			((Control)label33).set_TabIndex(35);
			((Control)label33).set_Text("Time");
			cmbMethod.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbMethod).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbMethod).set_FormattingEnabled(true);
			cmbMethod.get_Items().AddRange(new object[8] { "Unspecified", "Analogue or Digital video", "Digital SLR-camera video", "Photometer", "Sequential images", "Drift scan", "Visual", "Other" });
			((Control)cmbMethod).set_Location(new Point(85, 238));
			((Control)cmbMethod).set_Name("cmbMethod");
			((Control)cmbMethod).set_Size(new Size(175, 21));
			((Control)cmbMethod).set_TabIndex(34);
			cmbTime.set_DropDownStyle((ComboBoxStyle)2);
			((Control)cmbTime).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ListControl)cmbTime).set_FormattingEnabled(true);
			cmbTime.get_Items().AddRange(new object[8] { "Unspecified", "GPS", "NTP", "Telephone (fixed or mobile)", "Radio time signal", "Internal clock of recorder", "Stopwatch", "Other" });
			((Control)cmbTime).set_Location(new Point(85, 262));
			((Control)cmbTime).set_Name("cmbTime");
			((Control)cmbTime).set_Size(new Size(175, 21));
			((Control)cmbTime).set_TabIndex(36);
			((Control)label32).set_AutoSize(true);
			((Control)label32).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)label32).set_Location(new Point(35, 242));
			((Control)label32).set_Name("label32");
			((Control)label32).set_Size(new Size(49, 13));
			((Control)label32).set_TabIndex(33);
			((Control)label32).set_Text("Method");
			((Control)cmdToDetails).set_BackColor(Color.Cyan);
			((Control)cmdToDetails).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((Control)cmdToDetails).set_Location(new Point(197, 312));
			((Control)cmdToDetails).set_Name("cmdToDetails");
			((Control)cmdToDetails).set_Size(new Size(140, 37));
			((Control)cmdToDetails).set_TabIndex(16);
			((Control)cmdToDetails).set_Text("Transfer to Site");
			((ButtonBase)cmdToDetails).set_UseVisualStyleBackColor(false);
			((Control)cmdToDetails).add_Click((EventHandler)cmdToDetails_Click);
			((Control)cmdExit).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdExit).set_Font(new Font("Microsoft Sans Serif", 12f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdExit).set_Image((Image)Resources.error);
			((ButtonBase)cmdExit).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdExit).set_Location(new Point(856, 2));
			((Control)cmdExit).set_Name("cmdExit");
			((Control)cmdExit).set_Size(new Size(62, 27));
			((Control)cmdExit).set_TabIndex(17);
			((Control)cmdExit).set_Text("Exit");
			((ButtonBase)cmdExit).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdExit).set_UseVisualStyleBackColor(false);
			((Control)cmdExit).add_Click((EventHandler)cmdExit_Click);
			((Control)cmdHelp).set_BackColor(Color.DeepSkyBlue);
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 10f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(779, 2));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(63, 27));
			((Control)cmdHelp).set_TabIndex(18);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_TextAlign(ContentAlignment.MiddleRight);
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)panel4).set_BackColor(Color.AntiqueWhite);
			panel4.set_BorderStyle((BorderStyle)2);
			((Control)panel4).get_Controls().Add((Control)(object)cmdToDetails);
			((Control)panel4).get_Controls().Add((Control)(object)cmdAdd);
			((Control)panel4).get_Controls().Add((Control)(object)cmdDelete);
			((Control)panel4).get_Controls().Add((Control)(object)lstSites);
			((Control)panel4).set_Location(new Point(380, 33));
			((Control)panel4).set_Name("panel4");
			((Control)panel4).set_Size(new Size(538, 369));
			((Control)panel4).set_TabIndex(19);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Control)this).set_BackColor(Color.PaleTurquoise);
			((Form)this).set_ClientSize(new Size(929, 411));
			((Control)this).get_Controls().Add((Control)(object)panel4);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).get_Controls().Add((Control)(object)cmdExit);
			((Control)this).get_Controls().Add((Control)(object)panel3);
			((Control)this).get_Controls().Add((Control)(object)label3);
			((Control)this).get_Controls().Add((Control)(object)label1);
			((Control)this).set_Name("StandardSites");
			((Control)this).set_Text("Standard User Sites");
			((Form)this).add_FormClosing(new FormClosingEventHandler(StandardSites_FormClosing));
			((Control)panelDDD).ResumeLayout(false);
			((Control)panelDDD).PerformLayout();
			((Control)panelDMM).ResumeLayout(false);
			((Control)panelDMM).PerformLayout();
			((Control)panelDMS).ResumeLayout(false);
			((Control)panelDMS).PerformLayout();
			((Control)panel2).ResumeLayout(false);
			((Control)panel2).PerformLayout();
			((Control)panel1).ResumeLayout(false);
			((Control)panel1).PerformLayout();
			((Control)pnlObservers).ResumeLayout(false);
			((Control)pnlObservers).PerformLayout();
			((Control)panel3).ResumeLayout(false);
			((Control)panel4).ResumeLayout(false);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
