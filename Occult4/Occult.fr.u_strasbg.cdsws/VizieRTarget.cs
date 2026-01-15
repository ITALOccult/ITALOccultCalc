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
	public class VizieRTarget
	{
		private string nameField;

		private string positionField;

		private string referenceField;

		private bool sexaordecField;

		[SoapElement(IsNullable = true)]
		public string name
		{
			get
			{
				return nameField;
			}
			set
			{
				nameField = value;
			}
		}

		[SoapElement(IsNullable = true)]
		public string position
		{
			get
			{
				return positionField;
			}
			set
			{
				positionField = value;
			}
		}

		[SoapElement(IsNullable = true)]
		public string reference
		{
			get
			{
				return referenceField;
			}
			set
			{
				referenceField = value;
			}
		}

		public bool sexaordec
		{
			get
			{
				return sexaordecField;
			}
			set
			{
				sexaordecField = value;
			}
		}
	}
}
