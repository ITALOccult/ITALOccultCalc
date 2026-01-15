using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Lunar_Observations
{
	public class ErrorReport : Form
	{
		private IContainer components;

		internal TextBox txtErrors;

		public ErrorReport()
		{
			InitializeComponent();
		}

		private void ErrorReport_Load(object sender, EventArgs e)
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
			ErrorReport_Resize(sender, e);
		}

		private void ErrorReport_Resize(object sender, EventArgs e)
		{
			((Control)txtErrors).set_Top(3);
			((Control)txtErrors).set_Left(3);
			((Control)txtErrors).set_Width(((Control)this).get_Width() - 22);
			((Control)txtErrors).set_Height(((Control)this).get_Height() - 42);
		}

		private void ErrorReport_HelpButtonClicked(object sender, CancelEventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Data verification - Lunar observations");
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
			//IL_00d6: Unknown result type (might be due to invalid IL or missing references)
			//IL_00e0: Expected O, but got Unknown
			txtErrors = new TextBox();
			((Control)this).SuspendLayout();
			((Control)txtErrors).set_Location(new Point(7, 4));
			((TextBoxBase)txtErrors).set_Multiline(true);
			((Control)txtErrors).set_Name("txtErrors");
			txtErrors.set_ScrollBars((ScrollBars)3);
			((Control)txtErrors).set_Size(new Size(335, 165));
			((Control)txtErrors).set_TabIndex(0);
			((TextBoxBase)txtErrors).set_WordWrap(false);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(460, 169));
			((Control)this).get_Controls().Add((Control)(object)txtErrors);
			((Control)this).get_DataBindings().Add(new Binding("Location", (object)Settings.Default, "LocationLunarErrorReport", true, (DataSourceUpdateMode)1));
			((Form)this).set_HelpButton(true);
			((Form)this).set_Location(Settings.Default.LocationLunarErrorReport);
			((Form)this).set_MaximizeBox(false);
			((Form)this).set_MinimizeBox(false);
			((Control)this).set_Name("ErrorReport");
			((Control)this).set_Text("Data verification  -  error report");
			((Form)this).add_Load((EventHandler)ErrorReport_Load);
			((Form)this).add_HelpButtonClicked((CancelEventHandler)ErrorReport_HelpButtonClicked);
			((Control)this).add_Resize((EventHandler)ErrorReport_Resize);
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
