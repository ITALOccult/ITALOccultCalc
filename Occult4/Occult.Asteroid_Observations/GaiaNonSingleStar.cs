using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using Occult.Properties;

namespace Occult.Asteroid_Observations
{
	public class GaiaNonSingleStar : Form
	{
		private IContainer components;

		private Button cmdCopy;

		private Button cmdHelp;

		internal TextBox txtOut;

		public GaiaNonSingleStar()
		{
			InitializeComponent();
		}

		private void cmdHelp_Click(object sender, EventArgs e)
		{
			Help.ShowHelp((Control)(object)this, Utilities.AppPath + "\\Occult.chm", (HelpNavigator)(-2147483642), (object)"Gaia doubles");
		}

		private void cmdCopy_Click(object sender, EventArgs e)
		{
			Clipboard.SetText(((Control)this).get_Text() + "\r\n" + ((Control)txtOut).get_Text());
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
			cmdCopy = new Button();
			cmdHelp = new Button();
			txtOut = new TextBox();
			((Control)this).SuspendLayout();
			((Control)cmdCopy).set_BackColor(Color.Honeydew);
			((Control)cmdCopy).set_Location(new Point(65, 93));
			((Control)cmdCopy).set_Name("cmdCopy");
			((Control)cmdCopy).set_Size(new Size(78, 30));
			((Control)cmdCopy).set_TabIndex(18);
			((Control)cmdCopy).set_Text("Copy list");
			((ButtonBase)cmdCopy).set_UseVisualStyleBackColor(false);
			((Control)cmdCopy).add_Click((EventHandler)cmdCopy_Click);
			((Control)cmdHelp).set_BackColor(Color.SkyBlue);
			((Control)cmdHelp).set_Font(new Font("Microsoft Sans Serif", 8.25f, FontStyle.Bold, GraphicsUnit.Point, 0));
			((ButtonBase)cmdHelp).set_Image((Image)Resources.Help16x16);
			((ButtonBase)cmdHelp).set_ImageAlign(ContentAlignment.MiddleLeft);
			((Control)cmdHelp).set_Location(new Point(692, 93));
			((Control)cmdHelp).set_Name("cmdHelp");
			((Control)cmdHelp).set_Size(new Size(78, 30));
			((Control)cmdHelp).set_TabIndex(17);
			((Control)cmdHelp).set_Text("Help");
			((ButtonBase)cmdHelp).set_UseVisualStyleBackColor(false);
			((Control)cmdHelp).add_Click((EventHandler)cmdHelp_Click);
			((Control)txtOut).set_Font(new Font("Consolas", 8.25f, FontStyle.Regular, GraphicsUnit.Point, 0));
			((Control)txtOut).set_Location(new Point(2, 2));
			((TextBoxBase)txtOut).set_Multiline(true);
			((Control)txtOut).set_Name("txtOut");
			((Control)txtOut).set_Size(new Size(796, 85));
			((Control)txtOut).set_TabIndex(19);
			((ContainerControl)this).set_AutoScaleDimensions(new SizeF(6f, 13f));
			((ContainerControl)this).set_AutoScaleMode((AutoScaleMode)1);
			((Form)this).set_ClientSize(new Size(800, 129));
			((Control)this).get_Controls().Add((Control)(object)txtOut);
			((Control)this).get_Controls().Add((Control)(object)cmdCopy);
			((Control)this).get_Controls().Add((Control)(object)cmdHelp);
			((Control)this).set_Name("GaiaNonSingleStar");
			((Control)this).set_Text("GaiaNonSingleStar");
			((Control)this).ResumeLayout(false);
			((Control)this).PerformLayout();
		}
	}
}
