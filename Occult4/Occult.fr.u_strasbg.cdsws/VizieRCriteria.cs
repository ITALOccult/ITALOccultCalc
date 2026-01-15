using System;
using System.CodeDom.Compiler;
using System.ComponentModel;
using System.Diagnostics;
using System.Xml.Serialization;

namespace Occult.fr.u_strasbg.cdsws
{
	[Serializable]
	[GeneratedCode("System.Xml", "4.8.9037.0")]
	[DebuggerStepThrough]
	[DesignerCategory("code")]
	[SoapType(Namespace = "urn:VizieRBeta")]
	public class VizieRCriteria
	{
		private string astronomyField;

		private string authorField;

		private string catalogField;

		private string missionField;

		private string ucdField;

		private string wavelengthField;

		[SoapElement(IsNullable = true)]
		public string astronomy
		{
			get
			{
				return astronomyField;
			}
			set
			{
				astronomyField = value;
			}
		}

		[SoapElement(IsNullable = true)]
		public string author
		{
			get
			{
				return authorField;
			}
			set
			{
				authorField = value;
			}
		}

		[SoapElement(IsNullable = true)]
		public string catalog
		{
			get
			{
				return catalogField;
			}
			set
			{
				catalogField = value;
			}
		}

		[SoapElement(IsNullable = true)]
		public string mission
		{
			get
			{
				return missionField;
			}
			set
			{
				missionField = value;
			}
		}

		[SoapElement(IsNullable = true)]
		public string ucd
		{
			get
			{
				return ucdField;
			}
			set
			{
				ucdField = value;
			}
		}

		[SoapElement(IsNullable = true)]
		public string wavelength
		{
			get
			{
				return wavelengthField;
			}
			set
			{
				wavelengthField = value;
			}
		}
	}
}
